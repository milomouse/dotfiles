-- Ion statusbar module configuration file
-- milomouse <vincent[at]fea.st>

-- Create a statusbar
mod_statusbar.create{
    screen=0,
    pos='br',
    fullsize=true,
    systray=true,

    -- Space preceded by % adds stretchable space for alignment of variable
    -- meter value widths. > before meter name aligns right using this 
    -- stretchable space , < left, and | centers.
    -- Meter values may be zero-padded to a width preceding the meter name.
    -- These alignment and padding specifiers and the meter name may be
    -- enclosed in braces {}.
    --
    -- %filler causes things on the marker's sides to be aligned left and
    -- right, respectively, and %systray is a placeholder for system tray
    -- windows and icons.
    
    template="[ %linuxbatt%% ] %filler%mpd%systray",
    --template="%flashing %date | %load  %linuxbatt%%  %df  %wsname %exec_mchk %filler%mpd%systray",
    --template="[ %date || load: %05load_1min || mail: %02mail_new/%02mail_total ] %filler%systray",
}

-- Launch ion-statusd. This must be done after creating any statusbars
-- for necessary statusd modules to be parsed from the templates.
mod_statusbar.launch_statusd{
    date={
        date_format='%a %Y-%m-%d %H:%M',
    },
    load={
        --update_interval=10*1000,
        important_threshold=0.00,
        critical_threshold=2.00,
    },
    --[[exec={
	  mchk={
		 program = '/home/milo/code/shell/mailcheck',
		 retry_delay = 1 * 1000,
	 },
    },--]]
--[[    mail={
        update_interval=60*1000,
        mbox=os.getenv("mail"),
        files={ new = "/home/milo/mail/FastMail/INBOX" },
    }, --]]
}

