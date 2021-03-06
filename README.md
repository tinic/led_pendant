# led_pendant

This project is designed to compile on Debian or derivatives (Ubuntu, Linux Mint). Linux in a virtual machine will work fine (tested with VirtualBox). To compile and upload the firmware a few one time steps are required:

# One time setup
- Add your user ID to the dialout group as such (replace $USER with your username):
```
> sudo usermod -a -G dialout $USER
```
- Add a rule to set correct permissions on /dev/ttyUSB? devices and reboot:
```
> sudo echo > /etc/udev/rules.d/50-ttyusb.rules KERNEL==\"ttyUSB[0-9]*\",NAME=\"tts/USB%n\",SYMLINK+=\"%k\",GROUP=\"dialout\",MODE=\"0666\"
> reboot
```

- Bootstrap the compiler enviroment:
```
> cd led_pendant
> ./bootstrap.sh
```

# Compiling and debugging
- Plug the USB2serial converter attached to the LED pendant into your PC. Make sure you take over the USB device if you run Linux in a virtual machine. It should show up as a FTDI device.
- Finally build the project. The Makefile will automatically try to upload the firmware to the first /deb/ttyUSB? device it finds:
```
> make
```
- To get UART output from the LED pendant compile and run the leddebug tool. Note that for technical reasons running leddebug will always reset the device:
```
> make leddebug
> ./leddebug
```
