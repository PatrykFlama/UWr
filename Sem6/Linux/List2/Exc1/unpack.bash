mkdir -p linux_kernels_unpacked
for file in linux_kernels/*.tar.xz; do
	echo "Unpacking: $file"
	tar -xf "$file" -C linux_kernels_unpacked
done
