-- This File outlines the commands that the bot will use during operation
-- You may reload these commands by telling the bot to 'reload'
-- Comented lines begin with a '--'

--
-- Simply have the bot repeat everything after the word 'say'
--


Command { name    = "white"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayWhite"
        , desc    = "repeat a phrase in white"
        , trigger = [WordPresent "sayWhite"]
        , action  = [Respond Privmsg [ColorWhite (AllWordsAfter "sayWhite")] To_Current ] }

Command { name    = "black"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayBlack"
        , desc    = "repeat a phrase in black"
        , trigger = [WordPresent "sayBlack"]
        , action  = [Respond Privmsg [ColorBlack (AllWordsAfter "sayBlack")] To_Current ] }

Command { name    = "blue"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayBlue"
        , desc    = "repeat a phrase in blue"
        , trigger = [WordPresent "sayBlue"]
        , action  = [Respond Privmsg [ColorBlue (AllWordsAfter "sayBlue")] To_Current ] }

Command { name    = "green"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayGreen"
        , desc    = "repeat a phrase in green"
        , trigger = [WordPresent "sayGreen"]
        , action  = [Respond Privmsg [ColorGreen (AllWordsAfter "sayGreen")] To_Current ] }

Command { name    = "red"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayRed"
        , desc    = "repeat a phrase in red"
        , trigger = [WordPresent "sayRed"]
        , action  = [Respond Privmsg [ColorRed (AllWordsAfter "sayRed")] To_Current ] }

Command { name    = "brown"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayBrown"
        , desc    = "repeat a phrase in brown"
        , trigger = [WordPresent "sayBrown"]
        , action  = [Respond Privmsg [ColorBrown (AllWordsAfter "sayBrown")] To_Current ] }

Command { name    = "purple"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayPurple"
        , desc    = "repeat a phrase in purple"
        , trigger = [WordPresent "sayPurple"]
        , action  = [Respond Privmsg [ColorPurple (AllWordsAfter "sayPurple")] To_Current ] }

Command { name    = "orange"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayOrange"
        , desc    = "repeat a phrase in orange"
        , trigger = [WordPresent "sayOrange"]
        , action  = [Respond Privmsg [ColorOrange (AllWordsAfter "sayOrange")] To_Current ] }

Command { name    = "yellow"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayYellow"
        , desc    = "repeat a phrase in yellow"
        , trigger = [WordPresent "sayYellow"]
        , action  = [Respond Privmsg [ColorYellow (AllWordsAfter "sayYellow")] To_Current ] }

Command { name    = "lightGreen"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayLightGreen"
        , desc    = "repeat a phrase in lightGreen"
        , trigger = [WordPresent "sayLightGreen"]
        , action  = [Respond Privmsg [ColorLightGreen (AllWordsAfter "sayLightGreen")] To_Current ] }

Command { name    = "cyan"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayCyan"
        , desc    = "repeat a phrase in cyan"
        , trigger = [WordPresent "sayCyan"]
        , action  = [Respond Privmsg [ColorCyan (AllWordsAfter "sayCyan")] To_Current ] }

Command { name    = "aqua"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayAqua"
        , desc    = "repeat a phrase in aqua"
        , trigger = [WordPresent "sayAqua"]
        , action  = [Respond Privmsg [ColorAqua (AllWordsAfter "sayAqua")] To_Current ] }

Command { name    = "lightBlue"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayLightBlue"
        , desc    = "repeat a phrase in lightBlue"
        , trigger = [WordPresent "sayLightBlue"]
        , action  = [Respond Privmsg [ColorLightBlue (AllWordsAfter "sayLightBlue")] To_Current ] }

Command { name    = "pink"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayPink"
        , desc    = "repeat a phrase in pink"
        , trigger = [WordPresent "sayPink"]
        , action  = [Respond Privmsg [ColorPink (AllWordsAfter "sayPink")] To_Current ] }

Command { name    = "grey"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= sayGrey"
        , desc    = "repeat a phrase in grey"
        , trigger = [WordPresent "sayGrey"]
        , action  = [Respond Privmsg [ColorGrey (AllWordsAfter "sayGrey")] To_Current ] }

Command { name    = "silver"
        , state   = Active
        , auth    = [ACL_N]
        , usage   = ">>= saySilver"
        , desc    = "repeat a phrase in silver"
        , trigger = [WordPresent "saySilver"]
        , action  = [Respond Privmsg [ColorSilver (AllWordsAfter "saySilver")] To_Current ] }

