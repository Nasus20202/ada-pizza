@echo off
gprbuild -d -PC:\Users\Nasus\Desktop\Programowanie\Other\adapizza\adapizza.gpr C:\Users\Nasus\Desktop\Programowanie\Other\adapizza\src\simulation.adb
if errorlevel 1 goto end
 .\obj\simulation.exe
:end