-- A skeleton of a program for an assignment in programming languages The
-- students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; 
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   Product_Name: constant array (Product_Type) of String(1 .. 15)
     := ("Tire 205/55R 16", "Tire 195/60R 17", "Tire 215/50R 16",
         "Tire 205/55R 18", "Tire 225/55R 17");
   Production_Active: array (Product_Type) of Boolean
     := (True, True, True, True, True);
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 14)
     := ("Set of Tires 1", "Set of Tires 2", "Set of Tires 3");
   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);
   type My_Str is new String(1 ..256);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Consumer is
      -- Give the Consumer an identity
      entry Start(Consumer_Number: in Consumer_Type;
		    Consumption_Time: in Integer);
   end Consumer;

   -- In the Buffer, products are assemblied into an assembly
   task type Buffer is
      -- Accept a product to the storage provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
   end Buffer;

   Producers: array ( 1 .. Number_Of_Products ) of Producer;
   Consumers: array ( 1 .. Number_Of_Consumers ) of Consumer;
   Warehouse: Buffer;

   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production is new
      Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;	--  generator liczb losowych
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
      Accepted: Boolean := True;
   begin
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production.Reset(G);	--  start random number generator
         Product_Number := 1;
         Product_Type_Number := Product;
         Production := Production_Time;
      end Start;
      Put_Line("Started producer of " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(G)); --  symuluj produkcję
         if Accepted then
         Put_Line("Produced product " & Product_Name(Product_Type_Number)
                  & " number "  & Integer'Image(Product_Number));
         else
            Put_Line("Sending product " & Product_Name(Product_Type_Number)
                     & " number "  & Integer'Image(Product_Number)
                    & " one more time");
         end if;
         -- Accept for storage
         select
            Warehouse.Take(Product_Type_Number, Product_Number, Accepted);
         else
            Put_Line("Warehouse is not ready to take product at the moment");
            Accepted := False;
         end select;
         
         if Accepted then
            Product_Number := Product_Number + 1;
         end if;
      end loop;
   end Producer;

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;	--  random number generator (time)
      G2: Random_Assembly.Generator;	--  also (assemblies)
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
	of String(1 .. 10)
        := ("CarService", "CarTrack  ");
      Waiting_For_Assembly: Boolean := False;
   begin
      accept Start(Consumer_Number: in Consumer_Type;
		     Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);	--  ustaw generator
         Random_Assembly.Reset(G2);	--  też
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line("Started consumer " & Consumer_Name(Consumer_Nb));
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         if not Waiting_For_Assembly then
            Assembly_Type := Random_Assembly.Random(G2);
         end if;
           
         -- take an assembly for consumption
         select
            Warehouse.Deliver(Assembly_Type, Assembly_Number);
         else
            Put_Line("Warehouse is not ready no process any delivery");
            Assembly_Number := 0;
         end select;
         

		if (Assembly_Number = 0) then
            Put_Line(Consumer_Name(Consumer_Nb) & " should wait for his assembly: "
                     & Assembly_Name(Assembly_Type) & " to be prepared");
           Waiting_For_Assembly := True;
			delay Duration(5.0);
		else
         Put_Line(Consumer_Name(Consumer_Nb) & ": taken assembly " &
                    Assembly_Name(Assembly_Type) & " number " &
                    Integer'Image(Assembly_Number));
          Waiting_For_Assembly := False;
		end if;
      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity: constant Integer := 18;
      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((2, 1, 2, 1, 2),
            (2, 2, 0, 1, 0),
            (1, 1, 2, 0, 1));
      -- 5 4 4 2 3 sum 18
      -- 5/18 4/18 4/18 2/18 3/18
      Assembly_Proportion: array(Product_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;
      Sum_Of_All_Assemblies: Integer := 0;
      
      procedure Setup_Variables is
      begin

   
         for W in Product_Type loop
            Assembly_Proportion(W) := 0;
            for Z in Assembly_Type loop
               Assembly_Proportion(W) := Assembly_Proportion(W) + Assembly_Content(Z, W);
               Sum_Of_All_Assemblies := Sum_Of_All_Assemblies + Assembly_Content(Z, W);
            end loop;
         end loop;
         
         Put_Line("Warehouse can accept max: ");      
         for I in Product_Type loop
            Assembly_Proportion(I) := Assembly_Proportion(I) * Storage_Capacity / Sum_Of_All_Assemblies;
            Put_Line(Integer'Image(Assembly_Proportion(I)) & " pts of " & Product_Name(I));
         end loop;
         
         
         
      end Setup_Variables;

      function Can_Accept(Product: Product_Type) return Boolean is
      begin
         
         return Assembly_Proportion(Product) > Storage(Product);
         
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;
      
      function Is_Enough_For_Any_Assembly return Boolean is
         Any: array(Assembly_Type) of Boolean;
      begin
         
         for A in Assembly_Type loop
            Any(A) := True;
            for P in Product_Type loop
               if Storage(P) < Assembly_Content(A, P) then
                  Any(A) := False;
               end if;
            end loop;            
         end loop;
         
         for A in Assembly_Type loop
            if Any(A) then 
               return True;
            end if;
         end loop;
         
         return False;
      end Is_Enough_For_Any_Assembly;
      

      procedure Storage_Contents is
      begin
         for W in Product_Type loop
            Put_Line("Storage contents: " & Integer'Image(Storage(W)) & " "
                     & Product_Name(W));
         end loop;
      end Storage_Contents;

   begin
      Put_Line("Buffer started");
      Setup_Variables;
      loop
         select 
            when In_Storage < Storage_Capacity =>
            accept Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean) do
               if Can_Accept(Product) then
                  Put_Line("Accepted product " & Product_Name(Product) & " number " &
                             Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
                  Accepted := True;
               else
                  Put_Line("Rejected product " & Product_Name(Product) & " number " &
                             Integer'Image(Number));
                  Accepted := False;
               end if;
            end Take;
            Storage_Contents;
         or
            when Is_Enough_For_Any_Assembly =>
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line("Delivered assembly " & Assembly_Name(Assembly) & " number " &
                             Integer'Image(Assembly_Number(Assembly)));
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line("Lacking products for assembly " & Assembly_Name(Assembly));
                  -- TODO: add consumer to waiting queue 
                  Number := 0;
               end if;
            end Deliver;
            Storage_Contents;   
         end select;
         
      end loop;
   end Buffer;
   
begin
   for I in 1 .. Number_Of_Products loop
      Producers(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      Consumers(J).Start(J,12);
   end loop;
end Simulation;


