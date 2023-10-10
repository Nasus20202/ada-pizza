-- Symulator pizzerii
-- Autorzy:
-- Krzysztof Nasuta, 193328
-- Filip Dawidowski, 193433
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Simulation is
    type Ingredient_Type is
       (Dough, Cheese, Ham, Mushrooms, Tomato, Pepper, Pineapple);
    type Pizza_Type is
       (Margherita, Capriciosa, Hawaii, Pepperoni, Vegetariana);
    type Client_Type is (Student, Professor, Dean);
    package Random_Pizza is new Ada.Numerics.Discrete_Random (Pizza_Type);

    function To_String (Pizza : Pizza_Type) return String renames
       Pizza_Type'Image;
    function To_String (Ingredient : Ingredient_Type) return String renames
       Ingredient_Type'Image;
    function To_String (Client : Client_Type) return String renames
       Client_Type'Image;
    function To_String (Number : Integer) return String renames Integer'Image;

    -- Supplier produces determined ingredient
    task type Supplier_Task_Type is
        entry Start (Ingredient : in Ingredient_Type);
    end Supplier_Task_Type;

    -- Client gets pizza cooked from several ingredients from the Fridge
    task type Client_Task_Type is
        entry Start (Client : in Client_Type);
    end Client_Task_Type;

    -- In the Fridge, ingredients are cooked into pizzas
    task type Fridge_Task_Type is
        -- Accept a ingredient to the storage provided there is a room for it
        entry Insert
           (Ingredient : in     Ingredient_Type; Number : in Natural;
            Accepted   :    out Boolean);
        -- Cook and deliver a pizza provided there are enough ingredients for it
        entry Deliver
           (Pizza    : in     Pizza_Type; Number : out Natural;
            Accepted :    out Boolean);
    end Fridge_Task_Type;

    Supplier_Tasks : array (Ingredient_Type) of Supplier_Task_Type;
    Client_Tasks   : array (Client_Type) of Client_Task_Type;
    Fridge_Task    : Fridge_Task_Type;

    procedure Log (Logger : String; Message : String; Highlight: Boolean := False) is
        Offset : String (1 .. 25 - Logger'Length) := (others => ' ');
    begin
		if Highlight then
			Put_Line (ASCII.ESC & "[93m[" & Logger & "]" & Offset & Message & ASCII.ESC & "[0m");
		else
			Put_Line ("[" & Logger & "]" & Offset & Message);
		end if;
    end Log;

    task body Supplier_Task_Type is
        subtype Delay_Time_Range_Millis is Integer range 4_000 .. 7_000;
        package Random_Delay is new Ada.Numerics.Discrete_Random
           (Delay_Time_Range_Millis);
        Generator           : Random_Delay.Generator;
        Produced_Ingredient : Ingredient_Type;
        Counter             : Natural;
        Accepted            : Boolean := False;
        Retry_Delay         : Float   := 1.0;
    begin
        accept Start (Ingredient : in Ingredient_Type) do
            Random_Delay.Reset (Generator);
            Counter             := 1;
            Produced_Ingredient := Ingredient;
        end Start;
        Log ("Supplier " & To_String (Produced_Ingredient),
            "Started supplier");
        loop
            delay Duration (Float (Random_Delay.Random (Generator)) / 1_000.0);
            Log ("Supplier " & To_String (Produced_Ingredient),
                "Produced ingredient " & To_String (Produced_Ingredient) &
                " number " & To_String (Counter));

            while not Accepted loop
                select
                    Fridge_Task.Insert
                       (Produced_Ingredient, Counter, Accepted);
                else
                    Log ("Supplier " & To_String (Produced_Ingredient),
                        "Fridge is busy, waiting...");
                end select;
                if not Accepted then
                    delay Duration (Retry_Delay);
                end if;
            end loop;
            Counter  := Counter + 1;
            Accepted := False;
        end loop;
    end Supplier_Task_Type;

    task body Client_Task_Type is
        subtype Consumption_Time_Range_Millis is Positive range 6_000 .. 8_000;
        package Random_Consumption is new Ada.Numerics.Discrete_Random
           (Consumption_Time_Range_Millis);
        Time_Generator  : Random_Consumption.Generator;
        Pizza_Generator : Random_Pizza.Generator;
        Client_Name     : Client_Type;
        Counter         : Natural;
        Pizza           : Pizza_Type;
        Accepted        : Boolean;
    begin
        accept Start (Client : in Client_Type) do
            Random_Consumption.Reset (Time_Generator);
            Random_Pizza.Reset (Pizza_Generator);
            Client_Name := Client;
        end Start;
        Log ("Client " & To_String (Client_Name), "Started client");
        loop
            delay Duration
               (Float (Random_Consumption.Random (Time_Generator)) / 1_000.0);
            Pizza := Random_Pizza.Random (Pizza_Generator);
            Fridge_Task.Deliver (Pizza, Counter, Accepted);
            if Accepted then
                Log ("Client " & To_String (Client_Name),
                    "Received pizza " & To_String (Pizza) & " number " &
                    To_String (Counter), Highlight => True);
            else
                Log ("Client " & To_String (Client_Name),
                    "Can't buy " & To_String (Pizza));
            end if;
        end loop;
    end Client_Task_Type;

    task body Fridge_Task_Type is
        Storage_Capacity : constant Positive := 50;
        type Storage_type is array (Ingredient_Type) of Natural;
        Storage                : Storage_type := (others => 0);
        Pizza_Recipes : array (Pizza_Type, Ingredient_Type) of Natural :=
           (Margherita  => (Dough => 1, Cheese => 1, Tomato => 1, others => 0),
            Capriciosa  =>
               (Dough => 1, Cheese => 1, Ham => 2, Mushrooms => 2, Tomato => 2,
                others => 0),
            Hawaii      =>
               (Dough => 1, Cheese => 1, Ham => 1, Pineapple => 1, Tomato => 1,
                others => 0),
            Pepperoni   =>
               (Dough => 1, Cheese => 1, Pepper => 3, Ham => 1, others => 0),
            Vegetariana =>
               (Dough  => 1, Cheese => 1, Mushrooms => 2, Tomato => 2,
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
            Free         : Natural;
            -- how many ingredients are for ingrediention of arbitrary assembly
            Lacking      : array (Ingredient_Type) of Natural;
            -- how much room is needed in storage to produce arbitrary assembly
            Lacking_room : Natural;
            Result       : Boolean;
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
                return True;
            end if;
            if Integer'Max (0, Max_Ingredient_Content (Ingredient) - Storage (Ingredient)) > 0
            then
                return True;
            end if;
            Lacking_room := 1;
            for I in Ingredient_Type loop
                Lacking (I)  :=
                   Integer'Max (0, Max_Ingredient_Content (I) - Storage (I));
                Lacking_room := Lacking_room + Lacking (I);
            end loop;
            if Free >= Lacking_room then
                return True;
            else
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
            Message : Unbounded_String;
        begin
            Append (Message, "Ingredients: [ ");
            for I in Ingredient_Type loop
                Append (Message,
                    To_String (I) & ": " & To_String (Storage (I)) & " ");
            end loop;
            Append (Message, "]");
            Log ("Fridge", To_String (Message));
        end Storage_Contents;

        procedure Clear_Storage is
            type Maximal_Ingredient_Count_Type is
               array (Ingredient_Type) of Natural;
            Maximal_Ingredient_Count : Maximal_Ingredient_Count_Type :=
               (Dough => 9, Cheese => 9, Ham => 9, Tomato => 9, others => 6);
            Remove_Count             : Integer;
        begin
            for Ingredient in Ingredient_Type loop
                if (Storage (Ingredient) >
                    Maximal_Ingredient_Count (Ingredient))
                then
                    Remove_Count           := Storage (Ingredient) / 2;
                    Storage (Ingredient)   :=
                       Storage (Ingredient) - Remove_Count;
                    Ingredients_In_Storage :=
                       Ingredients_In_Storage - Remove_Count;
                    Log ("Fridge", "Removed " & To_String (Remove_Count) & " " &
                        To_String (Ingredient) & "S from storage");
                end if;
            end loop;
        end Clear_Storage;

    begin
        Log ("Fridge", "Started");
        Setup_Variables;
        loop
            select
                accept Insert
                   (Ingredient : in     Ingredient_Type; Number : in Natural;
                    Accepted   :    out Boolean)
                do
                    if Can_Accept (Ingredient) then
                        Log ("Fridge", "Accepted ingredient " & To_String (Ingredient) &
                            " number " & To_String (Number));
                        Storage (Ingredient)   := Storage (Ingredient) + 1;
                        Ingredients_In_Storage := Ingredients_In_Storage + 1;
                        Clear_Storage;
                        Storage_Contents;
                        Accepted := True;
                    else
                        Log ("Fridge", "Rejected ingredient " & To_String (Ingredient) &
                            " number " & To_String (Number));
                        Accepted := False;
                    end if;
                end Insert;
            or
                accept Deliver
                   (Pizza    : in     Pizza_Type; Number : out Natural;
                    Accepted :    out Boolean)
                do
                    if Can_Deliver (Pizza) then
                        Log ("Fridge", "Delivered pizza " & To_String (Pizza) &
                            " number " & To_String (Counters (Pizza)));
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
                        Accepted := True;
                    else
                        Number   := 0;
                        Accepted := False;
                    end if;
                end Deliver;
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
