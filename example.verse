# Example Verse code demonstrating syntax highlighting

using { /Verse.org/Simulation }
using { /Verse.org/Random }
using { /UnrealEngine.com/Temporary/Diagnostics }

# A simple game device
<epic_internal>
example_device := class<public>(creative_device):

    # Editable properties exposed in UEFN
    @editable
    var<public> PlayerSpawnLocation : vector3 = vector3{X := 0.0, Y := 0.0, Z := 100.0}
    
    @editable  
    var<public> MaxPlayers : int = 10

    # A function with various Verse language features
    <override><public>
    OnBegin<override>() : void =
        Print("Game device started!")
        
        # Variable declarations with types
        var PlayerCount : int = 0
        var GameActive : logic = true
        
        # Control flow with if/else
        if (PlayerCount > MaxPlayers):
            Print("Too many players: {PlayerCount}")
            set GameActive = false
        else:
            Print("Game ready with {PlayerCount} players")

        # Loop example
        for (I := 1..5):
            Print("Countdown: {I}")
            
        # Try-catch with concurrency
        try:
            spawn:
                DoSomethingAsync()
        catch:
            Print("Error occurred")

    # Private helper function
    <private>
    DoSomethingAsync<suspends>() : void =
        # String interpolation
        var Message : string = "Hello {PlayerSpawnLocation}"
        Print(Message)

# Enum example  
GameState := enum:
    Waiting
    Active  
    Finished

# Struct example
PlayerData := struct<public>:
    Name : string
    Score : int
    IsActive : logic

<# 
   Multi-line block comment
   This demonstrates block comment syntax
   <# nested comments are supported #>
#>