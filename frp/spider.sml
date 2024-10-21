
structure Spider =
struct

structure Weak = SMLofNJ.Weak
structure Susp = SMLofNJ.Susp
structure IntMap = IntRedBlackMap

structure Any =
   struct
      type t = exn

      fun new (x: 'a): t = let exception E of 'a in E x end
   end

type height = int

structure EventSubscribed =
   struct
      datatype t = T of {
         height: height ref,
         retained: Any.t,
         parents: unit -> t list,
         hasOwnHeight: bool,
         whoCreated: unit -> string list
      }

      fun height (T {height, ...}) = !height

      fun whoCreated (T {whoCreated, ...}) = whoCreated ()

      val zeroRef = ref 0

      fun root x = T {
         height = zeroRef,
         retained = Any.new x,
         parents = fn () => [],
         hasOwnHeight = false,
         whoCreated = fn () => ["root"]
      }

      val never = T {
         height = zeroRef,
         retained = Any.new (),
         parents = fn () => [],
         hasOwnHeight = false,
         whoCreated = fn () => ["never"]
      }

      val now = T {
         height = zeroRef,
         retained = Any.new (),
         parents = fn () => [],
         hasOwnHeight = false,
         whoCreated = fn () => ["now"]
      }

      val bogus = T {
            height = ref 0,
            retained = Any.new (),
            parents = fn () => raise Fail "EventSubscribed.bogus",
            hasOwnHeight = false,
            whoCreated = fn () => raise Fail "EventSubscribed.bogus"
         }
   end

structure EventSubscription =
   struct
      datatype t = T of {
         unsubscribe: unit -> unit,
         subscribed: EventSubscribed.t
      }

      (* Unsubscribe from the event *)
      fun unsubscribe (T {unsubscribe, ...}) = unsubscribe ()

      val never = T {unsubscribe = fn () => (), subscribed = EventSubscribed.never}

      (* Invalid subscription for lazy initialization *)
      val bogus = T {
            unsubscribe = fn () => raise Fail "EventSubscription.bogus",
            subscribed = EventSubscribed.bogus
         }
   end

structure Subscriber =
   struct
      datatype 'a t = T of {
            propagate: 'a -> unit,
            invalidateHeight: height -> unit,
            recalculateHeight: height -> unit
         }

      (* Modify the propagation function *)
      fun modifyPropagate (sub, k) =
         let
            val T {propagate, invalidateHeight, recalculateHeight} = sub
         in
            T {
               propagate = k propagate,
               invalidateHeight = invalidateHeight,
               recalculateHeight = recalculateHeight
            }
         end

      (* Create a terminal subscriber that never triggers other Events *)
      fun terminal p = T {
            propagate = p,
            invalidateHeight = fn _ => (),
            recalculateHeight = fn _ => ()
         }
   end

structure Event =
   struct
      datatype 'a t = T of 'a Subscriber.t -> EventSubscription.t * 'a option

      datatype 'a coincidence_subscribed = CoincidenceSubscribed of {
            cached: 'a coincidence_subscribed option ref,
            occurrence: 'a option ref,
            subscribers: 'a Subscriber.t WeakBag.t,
            height: height ref,
            outer: 'a t Subscriber.t,
            outerParent: EventSubscription.t,
            innerParent: EventSubscribed.t option ref,
            weakSelf: 'a coincidence_subscribed Weak.weak option ref
         }

      datatype 'a coincidence = Coincidence of {
            parent: 'a t t,
            subscribed: 'a coincidence_subscribed option ref
         }

      fun getCoincidenceSubscribed (c, sub) = raise Fail "NYI"
      fun eventSubscribedCoincidence subd = raise Fail "NYI"

      fun coincidence e =
         let
            val c = Coincidence {parent = e, subscribed = ref NONE}
            fun eventCoincidence sub =
               let
                  val (sln, subd, occ) = getCoincidenceSubscribed (c, sub)
                  val subscription =
                     EventSubscription.T {
                        unsubscribe = fn () => WeakBag.remove sln,
                        subscribed = eventSubscribedCoincidence subd
                     }
               in
                  (subscription, occ)
               end
         in
            T 
         end

      (* Subscribe to an event, returning the current occurrence *)
      fun subscribeAndRead (T k, sub) = k sub

      (* Subscription to an event that never occurs *)
      val subscribeAndReadNever = (EventSubscription.never, NONE)

      (* Map over an Event with no intermediate caching *)
      fun pushCheap (f: 'a -> 'b option) (e: 'a t): 'b t =
         T (fn sub =>
            let
               val sub' = Subscriber.modifyPropagate (sub, fn prop =>
                  fn a => Option.app prop (f a))
               val (subscription, occ) = subscribeAndRead (e, sub')
               val occ' = Option.mapPartial f occ
            in
               (subscription, occ')
            end)

      (* Subscribe to an event for one occurrence *)
      fun subscribeAndReadHead (e, sub) =
         let
            val subscriptionRef = ref EventSubscription.bogus
            val sub' = Subscriber.modifyPropagate (sub, fn prop =>
               fn a => (EventSubscription.unsubscribe (!subscriptionRef);
                        prop a))
            val (subscription, occ) = subscribeAndRead (e, sub')
         in
            case occ of
               NONE => subscriptionRef := subscription
             | SOME _ => EventSubscription.unsubscribe subscription;
            (subscription, occ)
         end

      val deferralQueue: (unit -> unit) list ref = ref []

      fun defer th = deferralQueue := th :: !deferralQueue

      (* Event that only fires on the first occurrence of the parent event *)
      fun head e =
         let
            val parent = ref (SOME e)
            fun clearParent () = parent := NONE
         in
            defer (fn () =>
               let
                  val sub = Subscriber.terminal (fn _ => clearParent ())
                  val (_, occ) = subscribeAndReadHead (e, sub)
               in
                  case occ of
                     NONE => ()
                   | SOME _ => clearParent ()
               end);
            T (fn sub =>
               case !parent of
                  NONE => subscribeAndReadNever
                | SOME e => subscribeAndReadHead (e, sub))
         end
   end

datatype invalidator =
   InvalidatorPull of unit
 | InvalidatorSwitch of unit

datatype subscribed = Subscribed of {
      height: height ref,
      retained: Any.t
   }

datatype subscription = Subscription of {
      unsubscribe: unit -> unit,
      subscribed: subscribed
   }

datatype 'a subscriber = Subscriber of {
      propagate: 'a -> unit,
      invalidateHeight: height -> unit,
      recalculateHeight: height -> unit
   }

(* type 'a eventM = unit -> 'a *)
datatype 'a event = Event of 'a subscriber -> subscription * 'a option

datatype ('a, 'p) hold = Hold of {
      value: 'a ref,
      invalidators: invalidator Weak.weak list ref,
      event: 'p event Susp.susp,
      parent: subscription option ref
   }

datatype ('a, 'p) dyn_type =
   UnsafeDyn of unit
 | BuildDyn of (unit -> 'a) * 'p event
 | HoldDyn of ('a, 'p) hold

datatype ('a, 'p) dyn = Dyn of ('a, 'p) dyn_type ref

datatype 'a coincidence_subscribed = CoincidenceSubscribed of {
      cached: 'a coincidence_subscribed option ref,
      occurence: 'a option ref,
      subscribers: 'a subscriber WeakBag.t,
      height: height ref,
      outer: 'a event subscriber,
      outerParent: subscription,
      innerParent: subscribed option ref,
      weakSelf: 'a coincidence_subscribed Weak.weak ref
   }

datatype 'a coincidence = Coincidence of {
      parent: 'a event event,
      subscribed: 'a coincidence_subscribed option ref
   }

local
   val zeroHeight = ref 0
   val subscribedNever = Subscribed {height = zeroHeight, retained = Any.new ()}

   fun eventSubscribedCoincidence (s as CoincidenceSubscribed {height, ...}) = Subscribed {
         height = height,
         retained = Any.new s
      }
in
val eventNever = Event (fn _ => (Subscription {unsubscribe = fn () => (), subscribed = subscribedNever}, NONE))
(* fun eventCoincidence c = Event (fn sub => *)
(*    let *)
(*    in *)
(*    end) *)
end

structure DeferQueue =
   struct
      fun deferAssignment (vRef: 'a ref, iRef: invalidator Weak.weak list ref, v: 'a): unit =
         ()

      fun deferHoldInit (apply: 'p * 'a -> 'a option, hold: ('a, 'p) hold): unit =
         ()

      fun deferDynInit (apply: 'p * 'a -> 'a option, dyn: ('a, 'p) dyn): unit =
         ()

      fun deferMergeUpdate (update: unit -> subscription list, invalidateHeight: unit -> unit, recalculateHeight: unit -> unit): unit =
         ()

      fun deferMergeInit (th: unit -> unit): unit =
         ()

      fun deferClear (r: 'a option ref): unit =
         ()

      fun deferIntClear (r: 'a IntMap.map ref): unit =
         ()

      fun deferRootClear (m: unit ref): unit = (* TODO *)
         ()

      fun deferResetCoincidence (sub: subscription, mcs: 'a coincidence_subscribed option): unit =
         ()
   end

local
   exception Now
   exception Never
   val zero = ref 0
in
   val eventSubscribedNow = Subscribed {height = zero, retained = Now}
   val eventSubscribedNever = Subscribed {height = zero, retained = Never}
end

fun deferAssignment (vRef: 'a ref, iRef: invalidator Weak.weak list ref, v: 'a): unit = ()

fun newSubscriberHold apply (Hold {value, invalidators, ...}) =
   Subscriber {
      propagate = fn a =>
         case apply (a, !value) of
            NONE => ()
          | SOME v' => deferAssignment (value, invalidators, v'),
      invalidateHeight = fn _ => (),
      recalculateHeight = fn _ => ()
   }

val _ : ('p * 'a -> 'a option) -> ('a, 'p) hold -> 'p subscriber = newSubscriberHold

structure EventEnv =
   struct
      datatype assignment = Assignment
      datatype hold_init = HoldInit

      val assignments: assignment list ref = ref []
      val holdInits: hold_init list ref = ref []

      val currentHeight = ref 0
      val delayedMerges: (unit -> unit) list IntRedBlackMap.map ref = ref IntRedBlackMap.empty
   end

end

(* vim: set tw=0 ts=3 sw=3: *)
