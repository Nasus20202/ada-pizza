-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of Suppliers, Clients, and the Fridge
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
    type Ingredient_Type is
       (Dough, Cheese, Ham, Mushrooms, Tomato, Pepper, Pineapple);
    type Pizza_Type is (Margherita, Capriciosa, Hawaii, Pepperoni);
    type Client_Type is (Student, Professor, Dean);
    package Random_Pizza is new Ada.Numerics.Discrete_Random (Pizza_Type);

	function ToString (Pizza : Pizza_Type) return String renames Pizza_Type'Image;
	function ToString (Ingredient : Ingredient_Type) return String renames Ingredient_Type'Image;
	function ToString (Client : Client_Type) return String renames Client_Type'Image;
	function ToString (Number : Integer) return String renames Integer'Image;

    -- Supplier produces determined ingredient
    task type Supplier_Task_Type is
        -- Give the Supplier an identity, i.e. the ingredient type
        entry Start (Ingredient : in Ingredient_Type);
    end Supplier_Task_Type;

    -- Client gets an arbitrary assembly of several ingredients from the Fridge
    task type Client_Task_Type is
        -- Give the Client an identity
        entry Start (Client : in Client_Type);
    end Client_Task_Type;

    -- In the Fridge, ingredients are assemblied into an assembly
    task type Fridge_Task_Type is
        -- Accept a ingredient to the storage provided there is a room for it
        entry Take (Ingredient : in Ingredient_Type; Number : in Natural);
        -- Deliver an assembly provided there are enough ingredients for it
        entry Deliver (Pizza : in Pizza_Type; Number : out Natural);
    end Fridge_Task_Type;

    Supplier_Tasks : array (Ingredient_Type) of Supplier_Task_Type;
    Client_Tasks   : array (Client_Type) of Client_Task_Type;
    Fridge_Task    : Fridge_Task_Type;

	procedure Log(Logger : String; Message : String) is
	Offset: String (1 .. 20 - Logger'Length) := (others => ' ');
	begin
        Put_Line("[" & Logger & "]" & Offset & Message);
	end Log;

    task body Supplier_Task_Type is
        subtype ingrediention_Time_Range is Positive range 4 .. 6;
        package Random_ingrediention is new Ada.Numerics.Discrete_Random
           (ingrediention_Time_Range);
        Generator : Random_ingrediention.Generator;   --  generator liczb losowych
        Produced_Ingredient : Ingredient_Type;
        Counter             : Natural;
    begin
        accept Start (Ingredient : in Ingredient_Type) do
            Random_ingrediention.Reset
               (Generator);    --  start random number generator
            Counter             := 1;
            Produced_Ingredient := Ingredient;
        end Start;
		Log("Supplier " & ToString(Produced_Ingredient), "Started supplier");
        loop
            delay Duration
               (Random_ingrediention.Random (Generator)); --  symuluj produkcję
            Log
               ("Supplier " & ToString(Produced_Ingredient), "Produced ingredient " &
                ToString(Produced_Ingredient) & " number " &
                ToString (Counter));
            -- Accept for storage
            Fridge_Task.Take (Produced_Ingredient, Counter);
            Counter := Counter + 1;
        end loop;
    end Supplier_Task_Type;

    task body Client_Task_Type is
        subtype Consumption_Time_Range is Positive range 6 .. 8;
        package Random_Consumption is new Ada.Numerics.Discrete_Random
           (Consumption_Time_Range);
        Time_Generator  :
           Random_Consumption.Generator;  --  random number generator (time)
        Pizza_Generator : Random_Pizza.Generator;    --  also (assemblies)
        Client_Name     : Client_Type;
        Counter         : Natural;
        Pizza           : Pizza_Type;
    begin
        accept Start (Client : in Client_Type) do
            Random_Consumption.Reset (Time_Generator);   --  ustaw generator
            Random_Pizza.Reset (Pizza_Generator);     --  też
            Client_Name := Client;
        end Start;
        Log ("Client " & ToString (Client_Name) , "Started client");
        loop
            delay Duration
               (Random_Consumption.Random
                   (Time_Generator)); --  simulate consumption
            Pizza := Random_Pizza.Random (Pizza_Generator);
            -- take an assembly for consumption
            Fridge_Task.Deliver (Pizza, Counter);
            if Counter /= 0 then
                Log
                   ("Client " & ToString (Client_Name), "took pizza " &
                    ToString (Pizza) & " number " &
                    ToString (Counter));
            else
                Log
                   ("Client " & ToString (Client_Name),
                    "lacking ingredients for assembly " &
                    ToString (Pizza));
            end if;
        end loop;
    end Client_Task_Type;

    task body Fridge_Task_Type is
        Storage_Capacity : constant Positive := 30;
        type Storage_type is array (Ingredient_Type) of Natural;
        Storage                : Storage_type := (others => 0);
        Pizza_Recipes : array (Pizza_Type, Ingredient_Type) of Natural :=
           (Margherita => (Dough => 1, Cheese => 1, Tomato => 2, others => 0),
            Capriciosa =>
               (Dough => 1, Cheese => 1, Ham => 2, Mushrooms => 2, Tomato => 1,
                others => 0),
            Hawaii     =>
               (Dough => 1, Cheese => 1, Ham => 2, Pineapple => 1, Tomato => 1,
                others => 0),
            Pepperoni  =>
               (Dough  => 1, Cheese => 1, Pepper => 2, Tomato => 1,
                others => 0));
        Max_Ingredient_Content : array (Ingredient_Type) of Natural;
        Counters : array (Pizza_Type) of Natural := (others => 1);
        Ingredients_In_Storage : Natural := 0;

        procedure Setup_Variables is
        begin
            for Ingredient in Ingredient_Type loop
                Max_Ingredient_Content (Ingredient) := 0;
                for Pizza in Pizza_Type loop
                    if Pizza_Recipes (Pizza, Ingredient) >
                       Max_Ingredient_Content (Ingredient)
                    then
                        Max_Ingredient_Content (Ingredient) :=
                           Pizza_Recipes (Pizza, Ingredient);
                    end if;
                end loop;
            end loop;
        end Setup_Variables;

        function Can_Accept (Ingredient : Ingredient_Type) return Boolean is
            Free         : Natural;         --  free room in the storage
            -- how many ingredients are for ingrediention of arbitrary assembly
            Lacking      : array (Ingredient_Type) of Natural;
            -- how much room is needed in storage to produce arbitrary assembly
            Lacking_room : Natural;
            Result       : Boolean;                   --  can accept
        begin
            if Ingredients_In_Storage >= Storage_Capacity then
                return False;
            end if;
            -- There is free room in the storage
            Free   := Storage_Capacity - Ingredients_In_Storage;
            Result := True;
            for I in Ingredient_Type loop
                if Storage (I) < Max_Ingredient_Content (I) then
                    Result := False;
                end if;
            end loop;
            if Result then
                return
                   True;                --  storage has ingredients for arbitrary
                --  assembly
            end if;
            if Integer'Max
                  (0,
                   Max_Ingredient_Content (Ingredient) -
                   Storage (Ingredient)) >
               0
            then
                -- exactly this ingredient lacks
                return True;
            end if;
            Lacking_room := 1;                     --  insert current ingredient
            for I in Ingredient_Type loop
                Lacking (I)  :=
                   Integer'Max (0, Max_Ingredient_Content (I) - Storage (I));
                Lacking_room := Lacking_room + Lacking (I);
            end loop;
            if Free >= Lacking_room then
                -- there is enough room in storage for arbitrary assembly
                return True;
            else
                -- no room for this ingredient
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
            Put ("[Fridge]              Ingredients: [ ");
            for I in Ingredient_Type loop
                Put (ToString (I) & ": " &
                    ToString (Storage(I)) & " ");
            end loop;
            Put_Line ("]");
        end Storage_Contents;

    begin
        Log ("Fridge", "Started");
        Setup_Variables;
        loop
            select
                accept Take
                   (Ingredient : in Ingredient_Type; Number : in Natural)
                do
                    if Can_Accept (Ingredient) then
                        Log("Fridge", "Accepted ingredient " &
                            ToString(Ingredient) & " number " &
                            ToString(Number));
                        Storage (Ingredient)   := Storage (Ingredient) + 1;
                        Ingredients_In_Storage := Ingredients_In_Storage + 1;
                        Storage_Contents;
                    else
                        Log("Fridge", "Rejected ingredient " &
                            ToString(Ingredient) & " number " &
                            ToString(Number));
                    end if;
                end Take;
            else
                null;
            end select;
            select
                accept Deliver (Pizza : in Pizza_Type; Number : out Natural) do
                    if Can_Deliver (Pizza) then
                        Log("Fridge", "Delivered pizza " & ToString(Pizza) &
                            " number " & ToString(Counters (Pizza)));
                        for Ingredient in Ingredient_Type loop
                            Storage (Ingredient)   :=
                               Storage (Ingredient) -
                               Pizza_Recipes (Pizza, Ingredient);
                            Ingredients_In_Storage :=
                               Ingredients_In_Storage -
                               Pizza_Recipes (Pizza, Ingredient);
                        end loop;
                        Number           := Counters (Pizza);
                        Counters (Pizza) := Counters (Pizza) + 1;
                        Storage_Contents;
                    else
                        Log("Fridge", "Lacking ingredients for pizza " &
                            ToString(Pizza));
                        Number := 0;
                    end if;
                end Deliver;
            else
                null;
            end select;
        end loop;
    end Fridge_Task_Type;

begin
    for Ingredient in Ingredient_Type loop
        Supplier_Tasks (Ingredient).Start (Ingredient);
    end loop;
    for Client in Client_Type loop
        Client_Tasks (Client).Start (Client);
    end loop;
end Simulation;
