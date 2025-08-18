with Ada.Text_IO; use Ada.Text_IO;
with Linux.Info.Battery; use Linux.Info.Battery;

package body Linux is
   procedure Get_ALL is begin
      Put_Line("Hello, I'm Linus");
   end Get_ALL;
end Linux;
