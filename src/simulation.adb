-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
	type Ingredient_Type is (Dough, Cheese, Ham, Mushrooms, Tomato, Salami, Onion, Pepper, Pineapple, Seafood);
	type Pizza_Type is (Margherita, Capriciosa, Hawaii, Salami, Pepperoni, Seafood);
	type Consumer_Type is (Student, Professor, Dean, Janitor);
    --Number_Of_Products   : constant Integer := 5;
    --Number_Of_Assemblies : constant Integer := 3;
    --Number_Of_Consumers  : constant Integer := 2;
    --subtype Product_Type is Integer range 1 .. Number_Of_Products;
    --subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
    --subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
    --Product_Name  : constant array (Product_Type) of String (1 .. 8)  :=
    --   ("Product1", "Product2", "Product3", "Product4", "Product5");
    --Assembly_Name : constant array (Assembly_Type) of String (1 .. 9) :=
    --   ("Assembly1", "Assembly2", "Assembly3");
    package Random_Pizza is new Ada.Numerics.Discrete_Random
       (Pizza_Type);
    --type My_Str is new String (1 .. 256);

    -- Producer produces determined product
    task type Producer_Task_Type is
        -- Give the Producer an identity, i.e. the product type
        entry Start (Ingredient : in Ingredient_Type);
    end Producer_Task_Type;

    -- Consumer gets an arbitrary assembly of several products from the buffer
    task type Consumer_Task_Type is
        -- Give the Consumer an identity
        entry Start
           (Consumer : in Consumer_Type);
    end Consumer_Task_Type;

    -- In the Buffer, products are assemblied into an assembly
    task type Buffer_Task_Type is
        -- Accept a product to the storage provided there is a room for it
        entry Take (Ingredient : in Ingredient_Type; Number : in Natural);
        -- Deliver an assembly provided there are enough products for it
        entry Deliver (Pizza : in Pizza_Type; Number : out Natural);
    end Buffer_Task_Type;

    Producer_Tasks : array (Ingredient_Type) of Producer_Task_Type;
    Consumer_Tasks : array (Consumer_Type) of Consumer_Task_Type;
    Buffer_Task    : Buffer_Task_Type;

    task body Producer_Task_Type is
        subtype Production_Time_Range is Positive range 3 .. 6;
        package Random_Production is new Ada.Numerics.Discrete_Random
           (Production_Time_Range);
        Generator : Random_Production.Generator;   --  generator liczb losowych
        Produced_Ingredient	 : Ingredient_Type;
        Counter      : Natural;
    begin
        accept Start (Ingredient : in Ingredient_Type)
        do
            Random_Production.Reset (Generator);    --  start random number generator
            Counter      := 1;
            Produced_Ingredient   := Ingredient;
        end Start;
        Put_Line ("Started producer of " & Ingredient_Type'Image(Produced_Ingredient));
        loop
            delay Duration
               (Random_Production.Random (Generator)); --  symuluj produkcję
            Put_Line
               ("Produced product " & Ingredient_Type'Image(Produced_Ingredient) &
                " number " & Integer'Image (Counter));
            -- Accept for storage
            Buffer_Task.Take (Produced_Ingredient, Counter);
            Counter := Counter + 1;
        end loop;
    end Producer_Task_Type;

    task body Consumer_Task_Type is
        subtype Consumption_Time_Range is Positive range 4 .. 8;
        package Random_Consumption is new Ada.Numerics.Discrete_Random
           (Consumption_Time_Range);
        Time_Generator : Random_Consumption.Generator;  --  random number generator (time)
        Pizza_Generator              : Random_Pizza.Generator;    --  also (assemblies)
        Consumer_Name     : Consumer_Type;
        Counter : Natural;
        Pizza   : Pizza_Type;
    begin
        accept Start
           (Consumer : in Consumer_Type)
        do
            Random_Consumption.Reset (Time_Generator);   --  ustaw generator
            Random_Pizza.Reset (Pizza_Generator);     --  też
            Consumer_Name := Consumer;
        end Start;
        Put_Line ("Started consumer " & Consumer_Type'Image(Consumer_Name));
        loop
            delay Duration
               (Random_Consumption.Random (Time_Generator)); --  simulate consumption
            Pizza := Random_Pizza.Random (Pizza_Generator);
            -- take an assembly for consumption
            Buffer_Task.Deliver (Pizza, Counter);
            Put_Line
               (Consumer_Type'Image(Consumer_Name) & ": taken assembly " &
                Pizza_Type'Image(Pizza) & " number " &
                Integer'Image (Counter));
        end loop;
    end Consumer_Task_Type;

    task body Buffer_Task_Type is
        Storage_Capacity : constant Positive := 30;
        type Storage_type is array (Ingredient_Type) of Natural;
        Storage              : Storage_type := (others => 0);
        Pizza_Recipes : array (Pizza_Type, Ingredient_Type) of Natural :=
           ((1, 2, 0, 0, 0, 0, 0, 0, 0, 0), (1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
			(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), (1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
			(1, 1, 1, 1, 0, 0, 0, 0, 0, 0), (1, 1, 1, 1, 1, 1, 1, 1, 1, 1));
        Max_Ingredient_Content : array (Ingredient_Type) of Natural;
        Counters      : array (Pizza_Type) of Natural := (others => 0);
        Ingredients_In_Storage           : Natural := 0;

        procedure Setup_Variables is
        begin
            for Ingredient in Ingredient_Type loop
                Max_Ingredient_Content (Ingredient) := 0;
                for Pizza in Pizza_Type loop
                    if Pizza_Recipes (Pizza, Ingredient) > Max_Ingredient_Content (Ingredient) then
                        Max_Ingredient_Content (Ingredient) := Pizza_Recipes (Pizza, Ingredient);
                    end if;
                end loop;
            end loop;
        end Setup_Variables;

        function Can_Accept (Ingredient : Ingredient_Type) return Boolean is
            Free         : Natural;         --  free room in the storage
            -- how many products are for production of arbitrary assembly
            Lacking      : array (Ingredient_Type) of Natural;
            -- how much room is needed in storage to produce arbitrary assembly
            Lacking_room : Natural;
            Result           : Boolean;                   --  can accept
        begin
            if Ingredients_In_Storage >= Storage_Capacity then
                return False;
            end if;
            -- There is free room in the storage
            Free := Storage_Capacity - Ingredients_In_Storage;
            Result   := True;
            for I in Ingredient_Type loop
                if Storage (I) < Max_Ingredient_Content (I) then
                    Result := False;
                end if;
            end loop;
            if Result then
                return True;                --  storage has products for arbitrary
                --  assembly
            end if;
            if Integer'Max (0, Max_Ingredient_Content (Ingredient) - Storage (Ingredient)) > 0
            then
                -- exactly this product lacks
                return True;
            end if;
            Lacking_room := 1;                     --  insert current product
            for I in Ingredient_Type loop
                Lacking (I)  :=
                   Integer'Max (0, Max_Ingredient_Content (I) - Storage (I));
                Lacking_room := Lacking_room + Lacking (I);
            end loop;
            if Free >= Lacking_room then
                -- there is enough room in storage for arbitrary assembly
                return True;
            else
                -- no room for this product
                return False;
            end if;
        end Can_Accept;

        function Can_Deliver (Pizza : Pizza_Type) return Boolean is
        begin
            for I in Ingredient_Type loop
                if Storage (I) < Pizza_Recipes (Pizza, I) then
                    return False;
                end if;
            end loop;
            return True;
        end Can_Deliver;

        procedure Storage_Contents is
        begin
            for I in Ingredient_Type loop
                Put_Line
                   ("Storage contents: " & Integer'Image (Storage (I)) & " " &
                    Ingredient_Type'Image(I));
            end loop;
        end Storage_Contents;

    begin
        Put_Line ("Buffer started");
        Setup_Variables;
        loop
            accept Take (Ingredient : in Ingredient_Type; Number : in Natural) do
                if Can_Accept (Ingredient) then
                    Put_Line
                       ("Accepted product " & Ingredient_Type'Image(Ingredient) &
                        " number " & Integer'Image (Number));
                    Storage (Ingredient) := Storage (Ingredient) + 1;
                    Ingredients_In_Storage        := Ingredients_In_Storage + 1;
                else
                    Put_Line
                       ("Rejected product " & Ingredient_Type'Image(Ingredient) &
                        " number " & Integer'Image (Number));
                end if;
            end Take;
            Storage_Contents;
            accept Deliver (Pizza : in Pizza_Type; Number : out Natural)
            do
                if Can_Deliver (Pizza) then
                    Put_Line
                       ("Delivered assembly " & Pizza_Type'Image (Pizza) &
                        " number " &
                        Integer'Image (Counters (Pizza)));
                    for Ingredient in Ingredient_Type loop
                        Storage (Ingredient) :=
                           Storage (Ingredient) - Pizza_Recipes (Pizza, Ingredient);
                        Ingredients_In_Storage  :=
                           Ingredients_In_Storage - Pizza_Recipes (Pizza, Ingredient);
                    end loop;
                    Number                     := Counters (Pizza);
                    Counters (Pizza) :=
                       Counters (Pizza) + 1;
                else
                    Put_Line
                       ("Lacking products for assembly " &
                        Pizza_Type'Image(Pizza));
                    Number := 0;
					Put_Line ("Waiting for products");
                end if;
            end Deliver;
            Storage_Contents;
        end loop;
    end Buffer_Task_Type;

begin
    for Ingredient in Ingredient_Type loop
        Producer_Tasks (Ingredient).Start (Ingredient);
    end loop;
    for Consumer in Consumer_Type loop
        Consumer_Tasks (Consumer).Start (Consumer);
    end loop;
end Simulation;
