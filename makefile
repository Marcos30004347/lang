all: build-conf

build-conf:
	if [ -d "./build" ]; then rm -rf ./build; fi
	mkdir build
	cd build && \
	cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=1  -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_FLAGS_DEBUG="-g -O0" -DCMAKE_CXX_FLAGS_DEBUG="-g -O0"
	if [ -f "./compile_commands.json" ]; then rm -rf ./compile_commands.json; fi
	ln ./build/compile_commands.json .

clean:
	if [ -d "./build" ]; then rm -rf ./build; fi
	if [ -d "./gauss_js_build" ]; then rm -rf ./gauss_js_build; fi
