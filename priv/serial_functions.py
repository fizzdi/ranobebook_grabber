#!/usr/bin/python
# -*- coding: utf-8 -*-

import serial
import socket
import time
import array
import select

def print_array(arr):
	print [x for x in arr];
	print ['%02x'%x for x in arr]
	try:
		print [ord(x) for x in arr];
	except:
		return 0;
	print "------"
	return 1;

def process_data(data):
    res = [];
    need_transform = 0;
    for x in data:
        ox = ord(x);
        if need_transform == 1:
            if ox == 0x00:
                res.extend([0xff]);
            elif ox == 0xfc:
                res.extend([0x03]);
            elif ox == 0xef:
                res.extend([0x10]);
            else:
                res.extend([0x10, ox]);
            need_transform = 0;
            continue;
        if ox <> 0x10:
            need_transform = 0;
            res.extend([ox]);
        else:
            need_transform = 1;
    return res;

def process_command(command):
    res = [];
    reserved = {0xff:[0x10, 0x00],
                0x03:[0x10, 0xfc],
                0x10:[0x10, 0xef]}
    for x in command:
        res.extend(reserved.get(x,[x]))
    return res;

def calcCRC(data):
    crc = 0
    for x in data:
        crc = crc ^ x;
    return crc;

def create_command(cmd, *args):
    start_byte = 0xff
    addr_to = 0x7f
    addr_from = 0x20
    pac = [cmd]
    for cur_data in args:
        pac.extend([cur_data]);
    prcom = process_command(pac);
    pac = [start_byte, addr_to, addr_from]
    pac.extend(prcom)
    pac.extend([calcCRC(pac), 0x03])
    return bytearray(pac);

def get_main_serial():
    return serial.Serial(port = "/dev/ttyr00", baudrate = 9600, parity = serial.PARITY_NONE, stopbits = serial.STOPBITS_ONE, bytesize = serial.EIGHTBITS, timeout = 1)
    
def get_all():
    cur_serial = get_main_serial()
    cur_serial.write(create_command(0x2e, 0x42))
    raw_data = cur_serial.read(24)
    data = process_data(raw_data)
    Weight = (data[6]<<8)+data[7]
    if (Weight > 32767):
        Weight-= 65536;
    cur_serial.close()
    return [Weight, str(data[11:18])]
	
def test():
    cur_serial = get_main_serial();
    cur_serial.write(create_command(0x4f))
    time.sleep(1.5)
    raw_data = cur_serial.read(254)
    cur_serial.close()

def result_func():
    cur_serial = get_main_serial()
    cur_serial.write(create_command(0x66, 0x01))
    res = cur_serial.read(9)
    cur_serial.close()
    return [res[5], res[6]];


def release_func():
    cur_serial = get_main_serial()
    cur_serial.write(create_command(0x66, 0x00))
    d = cur_serial.read(9)
    cur_serial.close()

def reset_weight():
    while result_func() != [0,0]:1
    cur_serial = get_main_serial()
    cur_serial.write(create_command(0x46, 0x5a))
    cur_serial.close()
    while result_func() != [0x5a, 80]:1

def get_weight_simple():
    cur_serial = serial.Serial(port = "/dev/ttyr01", baudrate = 9600, parity = serial.PARITY_NONE, stopbits = serial.STOPBITS_ONE, bytesize = serial.EIGHTBITS, timeout = 1)
    cur_serial.write('R')
    data = process_data(cur_serial.read(8))
    cur_serial.close()
    return data[2:8]

def get_t_rh():
    owen_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    owen_socket.connect(('192.168.19.69', 502))
    bytes = [0, 10, 0, 0, 0, 6, 2, 4, 0, 0, 0, 2]
    ready_to_read, ready_to_write, in_error = select.select([owen_socket, ], [owen_socket, ], [], 1)
    if len(ready_to_write) > 0:
        owen_command = "".join(map(chr, bytes))
        owen_socket.send(owen_command)
        time.sleep(0.2)
        ready_to_read, ready_to_write, in_error = select.select([owen_socket, ], [owen_socket, ], [], 1)
        if len(ready_to_read) > 0:
            data = owen_socket.recv(1024)
            data_array = array.array('B', data)
            t = (data_array[9] << 8) + data_array[10];
            if (t > 32767):
                t-= 65536;
            rh = (data_array[11] << 8) + data_array[12]
            owen_socket.close()
            return [t, rh]
    owen_socket.close()
    return ["undef", "undef"]
