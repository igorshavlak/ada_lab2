with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Calendar; use Ada.Calendar;

procedure Parallel_Min is


   Dim : constant Integer := 200000;

   Thread_Num : constant Integer := 2;

   Arr : array(1..Dim) of Integer;

   procedure Init_Arr is
        Gen : Generator;

   begin

      for I in 1..Dim loop

         Arr(I) := I;
      end loop;
      Arr(12345):=Arr(12345) * (-1);
   end Init_Arr;


   function Part_Min(Start_Index, Finish_Index : in Integer) return Integer is
      Min_Element : Integer := Arr(Start_Index);
   begin
      for I in Start_Index+1..Finish_Index loop
         if Arr(I) < Min_Element then
            Min_Element := Arr(I);
         end if;
      end loop;
      return Min_Element;
   end Part_Min;


   protected Part_Manager is
      procedure Set_Min_Element(Min_Element : in Integer);
      entry Get_Min_Element(Min_Element : out Integer);
   private
      Tasks_Count : Integer := 0;
      Min_Element_All : Integer := 0;
   end Part_Manager;

   protected body Part_Manager is

      procedure Set_Min_Element(Min_Element : in Integer) is
      begin
         if Tasks_Count = 0 or Min_Element < Min_Element_All then
            Min_Element_All := Min_Element;
         end if;
         Tasks_Count := Tasks_Count + 1;
      end Set_Min_Element;


      entry Get_Min_Element(Min_Element : out Integer) when Tasks_Count = Thread_Num is
      begin
         Min_Element := Min_Element_All;
      end Get_Min_Element;
   end Part_Manager;

   task type Starter_Thread is
      entry Start(Start_Index, Finish_Index : in Integer);
   end Starter_Thread;


   task body Starter_Thread is
      Min_Element : Integer := 0;
      Start_Index : Integer;
      Finish_Index : Integer;
   begin
      accept Start(Start_Index, Finish_Index : in Integer) do
         Starter_Thread.Start_Index := Start_Index;
         Starter_Thread.Finish_Index := Finish_Index;
      end Start;
      Min_Element := Part_Min(Start_Index => Start_Index,
                              Finish_Index => Finish_Index);
      Part_Manager.Set_Min_Element(Min_Element);
   end Starter_Thread;


   function Parallel_Min_Element return Integer is
      Min_Element : Integer := 0;
      Thread : array(1..Thread_Num) of Starter_Thread;
   begin

      for I in 1..Thread_Num loop
         Thread(I).Start((I-1) * (Dim / Thread_Num) + 1,
                         I * (Dim / Thread_Num));
      end loop;

      Part_Manager.Get_Min_Element(Min_Element);
      return Min_Element;
   end Parallel_Min_Element;
Time :Ada.Calendar.Time := Clock;
   Finish_time :Duration;
   rez:Integer;
begin

   Init_Arr;

   Time:=Clock;

   rez := Parallel_Min_Element;
   Finish_time:=Clock-time;
    Put("Min element: ");
    Put_Line(rez'img &" one thread time: " & Finish_time'img & " seconds");

end Parallel_Min;
