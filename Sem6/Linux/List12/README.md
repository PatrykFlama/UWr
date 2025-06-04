# List 12

## Zad3
```bash
truncate -s $((512 * 2147483648)) disk.img

LOOP=$(losetup --find --show disk.img)

```

sfdisk input file:
```
label: gpt
label-id: 7C54FB1F-0CEE-497F-9557-BCE60937DEF6
unit: sectors

# start       size           type
34            2014           type=21686148-6449-6E6F-744E-656564454649
2048          260096         type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B
262144        1835008        type=0FC63DAF-8483-4772-8E79-3D69D8477DE4
2097152       1053256064     type=CA7D7CCB-63ED-4C53-861C-1742536059CC
1065353216    8388608        type=0657FD6D-A4AB-43C4-84E5-0933C84B4F4F
1073741824    1073741791     type=824CC7A0-36A8-11E3-890A-952519AD3F61
```

```bash
sfdisk $LOOP < layout.txt
```

```bash
sgdisk \
  --partition-guid=1:CC053A2A-29B4-4D05-9E49-F2D4CD0C3000 \
  --partition-guid=2:B7E80F66-60D8-41C8-AD03-F54810C0CE42 \
  --partition-guid=3:81633C0B-0C11-4346-A2E5-5E7B46601BCB \
  --partition-guid=4:C3191E5B-D22B-4377-AF3E-C899A17E4CCA \
  --partition-guid=5:B056DC21-CD09-408A-A009-C28DE1E33DEC \
  --partition-guid=6:A5A73AB0-E418-4405-8466-70636A7F35B0 \
  $LOOP
```