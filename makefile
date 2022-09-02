all: build-conf

build-conf:
	if [ -d "./build" ]; then rm -rf ./build; fi
	mkdir build
	cd build && \
	cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=1  -D CMAKE_BUILD_TYPE=Debug
	if [ -f "./compile_commands.json" ]; then rm -rf ./compile_commands.json; fi
	ln ./build/compile_commands.json .

clean:
	if [ -d "./build" ]; then rm -rf ./build; fi
	if [ -d "./gauss_js_build" ]; then rm -rf ./gauss_js_build; fi
