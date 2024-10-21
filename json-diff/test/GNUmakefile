
CXX := g++
CXXFLAGS := -Wall -Wextra -std=c++11
LDFLAGS :=
LDLIBS := -lm

ieee754-conv: ieee754-conv.o
	$(CXX) $(LDFLAGS) $^ -o $@ $(LDLIBS)

ieee754-conv.o: ieee754-conv.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
