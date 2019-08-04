# Functional Specification


For Motors Module
                  
**Description**: The Motors module has three omni-wheels driven by DC motors, one laser diode, and one turret driven by a servo motor and a stepper motor. The omni-wheels allow the RoboTrike to move all directions without rotating. It uses a vector calculation to move with the desired speed and direction. The wheels are placed 120 degrees from each other on the circular board. The turret’s rotation is controlled by a stepper motor while the elevation is controlled by a servo motor. However, the turret’s movements are not implemented.
	It receives commands from the Remote module through the serial cable. It sends back the altered status to the Remote module if there’s any. It also transmits various error messages to the Remote module in case of errors. See the Error Handling Section for details.
	
**Global Variables**: None

**Input**:      Inputs are given through the serial cable (channel). The inputs are the commands in a string format. Thus, the Motors module parses the received command and executes a proper action.

**Output**:	The outputs of the Motors module are the DC motors(wheels), laser, and serial.
	Three DC motors are placed 120 degrees from each other on the circular board. Since the RoboTrike uses omni-wheels, a vector calculation is applied to move in a desired speed and direction. It calculates the pulse width for each speed and direction by a vector calculation and then uses PWM(Pulse Width Modulation) to actually control the motors. The equation used to obtain the pulse width is:
	PulseWidth= 〖Force〗_X*velocity*Cos(Angle)+ 〖Force〗_Y*velocity*Sin(Angle)
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
