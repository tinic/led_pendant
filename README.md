# led_pendant

This project is designed to compile on Debian or derivatives (Ubuntu, Linux Mint). To compile and upload the firmware a few steps are recommended to make it as easy as possible:

- Add your user ID to the dialout group as such (replace $USER with your username):
```
> sudo usermod -a -G dialout $USER
```
- Add a rule to set correct permission on /dev/ttyUSB? devices and reboot:
```
> sudo echo > /etc/udev/rules.d/50-myusb.rules KERNEL==\"ttyUSB[0-9]*\",MODE=\"0666\"
> reboot
```

- Bootstrap the compiler enviroment:
```
> cd led_pendant
> ./bootstrap.sh
```
- Attach the USB2serial converter attached to the LED pendant. Make sure you take over the USB device if you run Linux in a virtual machine. It should show up as a FTDI device.
- Finally build the project. The Makefile will automatically try to upload the firmware
```
> make
```
- To get UART output from the LED pendant compile and run the leddebug tool
```
> make leddebug
> ./leddebug
```
