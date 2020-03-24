
with Ada.Text_IO;
use  Ada.Text_IO;

with GNAT.Command_Line;
use  GNAT.Command_Line;

with Util;

with Ada.Exceptions;
use  Ada.Exceptions;

procedure Main is

  Config : Command_Line_Configuration;

begin

  Util.Define_CLI_Switches(Config);
  Getopt(Config);

  Util.Process_Command_Arguments;

exception
  when E : Util.Execution_Error =>
    Put("Execution error: ");
    Put_Line(Exception_Message(E));
    Put_Line("... stopping.");

  when E : Util.Argument_Error =>
    Put("Invalid argument: ");
    Put_Line(Exception_Message(E));
    Display_Help(Config);

  when Exit_From_Command_Line =>
    return;
end;