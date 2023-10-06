@echo off
gprbuild -d -Padapizza.gpr src\simulation.adb
if errorlevel 1 goto end
 .\obj\simulation.exe
:end