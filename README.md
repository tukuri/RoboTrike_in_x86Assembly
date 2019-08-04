# Robotrike in x86 Assembly (Caltech EE/CS51) by Sung Hoon Choi 

## Functional Specification for the Motors module
                  
**Description**: The Motors module has three omni-wheels driven by DC motors, one laser diode, and one turret driven by a servo motor and a stepper motor. The omni-wheels allow the RoboTrike to move all directions without rotating. It uses a vector calculation to move with the desired speed and direction. The wheels are placed 120 degrees from each other on the circular board. The turret’s rotation is controlled by a stepper motor while the elevation is controlled by a servo motor. However, the turret’s movements are not implemented.
	It receives commands from the Remote module through the serial cable. It sends back the altered status to the Remote module if there’s any. It also transmits various error messages to the Remote module in case of errors. See the Error Handling Section for details.
	
**Global Variables**: None

**Input**:      Inputs are given through the serial cable (channel). The inputs are the commands in a string format. Thus, the Motors module parses the received command and executes a proper action.

**Output**:	The outputs of the Motors module are the DC motors(wheels), laser, and serial.
	Three DC motors are placed 120 degrees from each other on the circular board. Since the RoboTrike uses omni-wheels, a vector calculation is applied to move in a desired speed and direction. It calculates the pulse width for each speed and direction by a vector calculation and then uses PWM(Pulse Width Modulation) to actually control the motors. The equation used to obtain the pulse width is:
	
_PulseWidth= Force<sub>x</sub> * velocity * Cos(&theta;) + Force<sub>y</sub> * velocity * Sin(&theta;)_
	
The Motors module turns the laser on if it gets the command to turn it on. It           turns the laser off if it gets the command to turn it off.
Also, it transmits the altered status of the RoboTrike back to the Remote module after executing an action. For example, if the speed has been changed by the last command, it sends the updated speed value back to the Remote module so that the status can be displayed on LED digits. See the Functional Specification for Remote to check actual messages shown on LED display. It also sends error messages to the Remote module in case of errors. There are three types of errors: System Failure, Serial Error, and Parser Error. See the Error Handling section for details. 
 

**User Interface**: The user cannot control the Motors module directly.
	  They should use the Remote module to control the Motors module.

**Error Handling**: Errors messages will be sent to the Remote Module in case of errors.

| Error Type | Error Message | Error Detail | How to resolve it |
|----------|-------------|------------|-----------------|
|System Failure|	‘SYS_FAIL’|	System Failure occurs when the EventQueue of RoboTrike is full.|	The system will reset automatically once there’s a system failure.|
|Serial Error|	‘SEri_Err’ | Serial Error occurs when there’s an error on the serial communication.| 	Check the parity and baud rates and fix them if they are incorrect.|
|Parser Error|	‘PArS_Err’|	Parser Error occurs when the parser function generates an error.|It means that an illegal command has been ordered.	Check the parity and baud rates and fix them if they are incorrect.|

**Algorithms**: It uses the vector calculation and Pulse Width Modulation to control the DC motors. Also, it uses the Finite State Machine to parse the received commands.

**Data Structures**: StatusBuffer and MotorTxBuffer were used to transmit the updated status value and error messages to the Remote module. Also, various states of the Finite State Machine were used for parsing the commands.

**Limitations**: Since there is no feedback in this system, there’s no way to tell if the system moved the correct distance or direction. Also, since the Motor module is connected to the Remote module by a wire(serial cable), the RoboTrike cannot move freely.

**Known Bugs**: None

**Special Notes**: Wireless communication between the Motors module and the Remote module should be implemented to enable free movements of the RoboTrike.

## Functional Specification for the remote module

**Description**: The remote module consists of eight 7-segment LED digit displays and 16 (4 keys per row) keys. When the user pushes a key, the corresponding command will be transmitted to the Motors module through the serial channel. Also, every time a command is transmitted to the Motors module, the changed status(speed, direction, laser) will be displayed on the LED digits. If the command did not change any status of the RoboTrike, LED digits will keep displaying the old message. It also displays various error messages in case of errors and resets in case of the system failure. See the Error Handling section for details. Note that the auto-repeat is implemented for the keys, so the user can keep pressing a key to execute a command for a large number of times.

**Global Variables**:  None

**Input**: Majority of inputs are given through the keys. There are 16 (4x4) keys on the Remote module. See the following descriptions for each key.
 
|Key Number|	Key Name|	Description|
|---|--|--|
|1|	Set Half Maximum Speed|	Set the RoboTrike’s speed to the half of maximum value.|
|2|	Stop|	Stop the RoboTrike’s motors.|
|3|	Change direction 45 deg clockwise	|Alter the direction 45 degrees clockwise.|
|4|	Change direction 45 deg Anti-clockwise|	Alter the direction 45 degrees anti-clockwise.|
|5|	Increment Speed (Delicate)|	Increment the speed a little bit. (Increments 1.5% the MAX speed each time a key is pressed. Use Auto-Repeat if you want a huge change in speed. The speed will not increment if it reached the MAX speed)|
|6|	Increment Speed(Rough)|	Increment the speed considerably.(Increments 7.5% the MAX speed each time a key is pressed. Use Auto-Repeat if you want a huge change in speed. The speed will not increment if it reached the MAX speed)|
|7|	Decrement Speed (Delicate)|	Decrement the speed a little bit. (Decrements 1.5% the MAX speed each time a key is pressed. Use Auto-Repeat if you want a huge change in speed. The speed will not decrement if it reached 0.)|
|8|	Decrement Speed (Rough)|	Decrement the speed considerably. (Decrements 7.5% the MAX speed each time a key is pressed. Use Auto-Repeat if you want a huge change in speed. The speed will not decrement if it reached 0.)|
|9|	Turn Turret 45 deg Clockwise (To be implemented)|	Turn the turret’s angle 45 degrees clockwise. (To be implemented)|
|10|	Turn Turret 45 deg Anti-Clockwise (To be implemented)|	Turn the turret’s angle 45 degrees anti-clockwise.(To be implemented)|
|11|	Turn Turret Elevation 30 deg up (To be implemented)|	Turn the turret’s elevation 30 degrees upward. (To be implemented)|
|12|	Turn Turret Elevation 60 deg up (To beimplemented)|	Turn the turret’s elevation 60 degrees upward.(To be implemented)|
|13|	Turn Turret Elevation 30 deg down (To beimplemented)|	Turn the turret’s elevation 30 degrees downward.(To be implemented)|
|14|	Turn Turret Elevation 60 deg down (To be implemented)|	Turn the turret’s elevation 60 degrees downward.(To be implemented)|
|15|	Turn Laser On|	Turn the laser on.|
|16|	Turn Laser Off|	Turn the laser off.|

The Remote module also receives Error Message strings from the Motors module as an input. For actual error messages, please see the Error Handling section.

**Output**:	Eight 7-segment LED digits are used to display the recently updated status of the RoboTrike. For example, let’s say the current direction of the RoboTrike is +45 degrees(from the center line of the Motors Module). Now, you pressed Key#3. Then, the LED digits will display ‘D00090’. Note that the LED digits always display 6 characters. Thus, zeroes will be padded if the number has less than 5 digits. The format of the status message on LED is
	“Status Type” + “Status Value”
	While there are three status types: 
	S - Speed
	D - Direction
	L – Laser
Note that the range of speed is [0, 65534] and the range of direction is [0, 359]. For the laser, Status Value of 1 indicates that the laser is turned on while Status Value of 0 indicates that the laser is turned off.
Also, it displays ‘SEri_Err’ for a serial error, ‘PArS_Err’ for a parser error, and ‘SYS_FAIL’ for the system failure. See Error Handling section for details.

**User Interface**: Users can send commands to the Motors module by pressing the keys on the Remote module. Once the user presses the key, the altered status of the RoboTrike will be displayed on the LED digits. If the command did not change any status of RoboTrike, the message displayed on the LED digits won’t change. The user can order multiple commands without pressing keys tremendous times since the Auto-Repeat is implemented on the keys. The user can keep pressing the key and the same command will be transmitted about 5 times per second. The user can notice if there’s an error on the RoboTrike by reading the message on LED digits. The LED digits display proper error messages for each type of error. If the system failure occurs, the system will reset automatically.

**Error Handling**: Error messages will be displayed on the LED digits in case of errors.

|Error Type|Error Message|Error Detail|	How to resolve it|
|----------|-------------|------------|------------------|
|System Failure|‘SYS_FAIL’|System Failure occurs when the EventQueue of RoboTrike is full.|The system will reset automatically once there’s a system failure.|
|Serial Error|‘SEri_Err’|Serial Error occurs when there’s an error on the serial communication.|Check the parity and baud rates and fix them if they are incorrect.|
|Parser Error|‘PArS_Err’|Parser Error occurs when the parser function generates an error. It means that an illegal command has been ordered.|Check the parity and baud rates and fix them if they are incorrect.|


**Algorithms**:  7-segment LED digits use multiplexing to display proper data on LED. 
	
**Data Structures**: Event queues were used to handle the events happening on the RoboTrike. Also, the RemoteDisplayBuffer was used to handle the strings to be displayed on LED digits. 

**Limitations**: Since there is no feedback in this system, there’s no way to tell if the system moved the correct distance or direction. Also, since the RoboTrike uses 7-segment LED digits, it cannot display certain types of characters.

**Known Bugs**: None

**Special Notes**: None

