with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Semaphores; use GNAT.Semaphores;

procedure Dinner_Philosophers is
   task type Philosopher is
      entry Start(Id : Integer);
   end Philosopher;

   Forks : array (1..5) of Counting_Semaphore(1, Default_Ceiling);
   Waiter : array (1..2) of Counting_Semaphore(1, Default_Ceiling);

   task body Philosopher is
      Id : Integer;
      Id_Left_Fork, Id_Right_Fork : Integer;
   begin
      accept Start (Id : in Integer) do
         Philosopher.Id := Id;
      end Start;
      Id_Left_Fork := Id;
      Id_Right_Fork := Id rem 5 + 1;

      loop
         Waiter(1).Seize;

         Forks(Id_Left_Fork).Seize;
         Put_Line("Philosopher " & Integer'Image(Id) & " took left fork");

         Forks(Id_Right_Fork).Seize;
         Put_Line("Philosopher " & Integer'Image(Id) & " took right fork");

         Put_Line("Philosopher " & Integer'Image(Id) & " is dining.");

         Forks(Id_Left_Fork).Release;
         Put_Line("Philosopher " & Integer'Image(Id) & " put left fork");
         Forks(Id_Right_Fork).Release;
         Put_Line("Philosopher " & Integer'Image(Id) & " put right fork");
         Waiter(1).Release;

         delay 1.0;
      end loop;
   end Philosopher;

   Philosophers : array (1..5) of Philosopher;
begin
   for I in Philosophers'Range loop
      Philosophers(I).Start(I);
   end loop;

   delay 10.0;
end Dinner_Philosophers;
