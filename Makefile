.PHONY: all

all: convert-vms-record-fmt

convert-vms-record-fmt: convert-vms-record-fmt.c
	gcc -o convert-vms-record-fmt convert-vms-record-fmt.c
