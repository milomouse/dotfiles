IRB_START_TIME = Time.now

require 'rubygems'
require 'irb/completion'
require 'pp'
require 'map_by_method'
require 'irb/ext/save-history'
require 'wirble'
Wirble.init
Wirble.colorize

IRB.conf[:AUTO_INDENT] = true

IRB.conf[:PROMPT][:MINE] = {
  :PROMPT_I => ">> ",
  :PROMPT_S => "~> ",
  :PROMPT_C => "*> ",
  :RETURN => "=>%s\n"
}

IRB.conf[:PROMPT_MODE] = :MINE
