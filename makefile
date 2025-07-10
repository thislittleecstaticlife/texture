# makefile : texture

target = texture
cxx = clang++
cxxflags = -Wall -Wextra -Wpedantic -std=c++2c -stdlib=libc++ -funsigned-char
lflags =
objects = main.o
stdmodulecache = std.pcm

$(target): $(objects)
	$(cxx) -o $(target) $(cxxflags) $(lflags) $(objects)

%.o: %.cpp $(stdmodulecache)
	$(cxx) -c -o $@ $(cxxflags) -fmodule-file=std=$(stdmodulecache) $<

$(stdmodulecache): /usr/share/libc++/v1/std.cppm
	$(cxx) $(cxxflags) \
	-Wno-reserved-identifier \
	-Wno-reserved-module-identifier \
	--precompile \
	-o $@ $<

.PHONY: stdmodule
stdmodule: $(stdmodulecache)

.PHONY: test
test: $(target)
	./$(target)

.PHONY: clean
clean:
	rm -f $(target) *.o

.PHONY: allclean
allclean: clean
	rm -f $(stdmodulecache)

