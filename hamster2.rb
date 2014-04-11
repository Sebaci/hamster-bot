# encoding: UTF-8
#!/usr/bin/env ruby
# encoding below is for ruby 1.8.7; deprecated in newer versions
$KCODE = "UTF-8"

require 'rubygems' # ruby 1.8.7
require 'open-uri'
require 'nokogiri'
require 'yaml/dbm'
require 'socket'
require './irc-format'

VER = '2.6.2 alpha'

# http://www.erepublik.com/en/military/battlefield-choose-side/45722/35  D3 - 20 (suma:130) || join PL
# nie przekazano

# join - sprawdzic

# "zamykam na T15" - wywalic

class Hamster
  def initialize server, port
    @socket = TCPSocket.open server, port
    @msgqueue = []
    
    db = YAML::DBM.open 'hdata'
    
    @pafchan = db['pafchan']
    @dowchan = db['dowchan']
    @dowpass = db['dowpass']
    
    @egov = db['egov']
    @battleLink = db['battleLink']
    @battleSide = db['battleSide']
    
    #array of orders for each division
    @divOrders = db['divOrders']
    #array of 4 arrays of nicks
    @divNames = db['divNames']
    # array of 5 timestamps (all divs,d1,2,3,4)
    @gatherTimes = db['gatherTimes']
    
    @customCmds = db['customCmds']
    
    @seenNames = db['seenNames']
    @monOrders = db['monOrders']
    
    auth = db['auth']
    
    db.close
    
    File.open('commands.yaml', 'r') {|file| @cmdsDescr = YAML.load file}
    File.open('davila.yaml', 'r') {|file| @davila = YAML.load file}
    
    @names = Hash.new
    
    say "NICK Hamster[PAF]"
    say "USER Hamster[PAF] Hamster[PAF] Hamster[PAF] :HamsterPAF"
    
    notices, pingreply = 0, false
    begin
      msg = @socket.gets
      puts msg
      if msg.match /^PING :(.*)$/ then
        say "PONG #{$~[1]}"
        pingreply = true
      elsif msg.match /NOTICE/ then notices += 1 end
    end until pingreply && notices == 4    
      
    say "PRIVMSG Q@Cserve.quakenet.org :AUTH HamsterPAF #{auth}"
    
    begin
      msg = @socket.gets
      puts msg
    end until msg.match /(You are now logged in|AUTH is not available once)/
    
    join @pafchan
    join @dowchan, @dowpass
  end
  
  def join channel, pass = ''
    say "JOIN #{channel} #{pass}".rstrip
    @names[channel] = []
    loop do
      msg = @socket.gets.rstrip
      puts msg
      break if msg.match /End of \/NAMES list\./
      if msg.match /[\=@] #{channel} :(.+)$/
        names = $~[1]
        @names[channel] += names.scan /\S+/
      else @msgqueue.push msg end
    end
  end
  
  def say msg
    puts msg
    @socket.puts msg
  end
  
  def pafsay msg
    say "PRIVMSG #{@pafchan} :#{msg}"
  end
  
  def massslap divs = nil
    names = @names[@pafchan].map do |name|
      if name.match /^[@+](.+)$/ then $~[1] else name end
    end
    unless divs.nil?
      divnames = []
      divs.each {|div| divnames.concat @divNames[div-1]}
      names &= divnames
    end
    
    names = names.join ' '
    pafsay "PIF PAF! #{names}"
  end
  
  def checkMode user, modesStr, channel
    @names[channel].any? {|n| n.match /[#{modesStr}]#{Regexp.escape(user)}/ }
  end
  
  def cmdDescr cmd
    @cmdsDescr[cmd].join ' -- '
  end
  
  def ordersString div = :all
    orders = []
    @divOrders.each_with_index do |dorder,i|
      next if div != :all && div-1 != i
      next if dorder.nil?
      d = "Div#{i+1}".bold
      order = dorder.to_s.bold.red
      order.concat " hits".bold.red if dorder.is_a? Integer
      orders.push "#{d} - #{order}"
    end
    
    orders.join ' | '.bold
  end
  
  def paforder orderStr
    parseError = false
    order = orderStr.normal.gsub(/egova?/i, '').gsub(/opens?/i,'otwiera')
    order.gsub!(/closes?/i, 'zamyka')
    
    # change ranges like 'd2-d4' to 'd2 d3 d4'
    ranges = order.scan /(d(\d)\s?-\s?d?(\d))(?!\s*(?:\d*\s?hit|min))/i
    ranges.each {|rng| order.sub!(rng[0], (rng[1]..rng[2]).map{|d| "d#{d}"}.join(' '))}
    
    if order.match /(http\S+)\s+(.+)$/i
      @battleLink, order = $~[1], $~[2]
      if order.match /(join\s+\w+)/i
        @battleSide = $~[1]
        order.sub! @battleSide, ''
      else @battleSide = nil end
    end
    
    # this seems weird, but sometimes helps to recognise in case 'hits' is missing
    order.gsub! /\([^)]+\)/, 'hits'
puts "ORDER"; p order
    
    if order =~ /di?v?\s*[1-4]/i && !(order =~ /(all|wszys\w+)\s+d\w*/i)
      divs = []

      dreg = /(?:d(?:iv)?\s*[1-4](?:\s*(?:[,&i|]|and|oraz)?(?:\s*[1-4](?!\s*(?:\d+|hit|min))|\s*d(?:iv)?\s*[1-4]))*)/i
      treg = /\d+\D*hit\w*|all?\s*ina?|otwiera\w*|zamyka\w*,?[-\s]+\w{0,2}\s*\d+\s*min\w*|zamyka\w*|min(?:$|[^i])/i
      
      orderDivs = order.scan dreg
      order.gsub! dreg, ''
      orderTypes = order.scan treg
puts "DIVS"; p orderDivs; puts "TYPES"; p orderTypes; puts "DIVORDERS"; p @divOrders
      if orderDivs.length != orderTypes.length
        parseError = true
      else
        orders = orderDivs.zip orderTypes

        orders.each do |oDivs, type|
          if type.match /(\d+)\D*hit/i then divOrder = $~[1].to_i
          elsif type.match /otwier/i then divOrder = 'otwiera'
          elsif type.match /min/i then divOrder = 'zamyka'
          elsif type.match /zamyk/i then divOrder = nil
          elsif type.match /all?\s*in/i then divOrder = 'allin' end
          
          oDivs.scan /\d/ do |div|
            d = div.to_i
            divs.push d unless divs.include? d
            
            if divOrder.is_a? Integer
              @divOrders[d-1] = @divOrders[d-1].to_i + divOrder
            else @divOrders[d-1] = divOrder end
          end
        end
        divs.sort!
      end
    
    else
      divs = []
      @divOrders.each_with_index {|div,i| divs.push i+1 unless div.nil?}
      case order
        when /(\d+)\D*hit/i
          hits = $~[1].to_i
          divs.each {|div| @divOrders[div-1] = @divOrders[div-1].to_i + hits}
        when /all?\s*in/i
          divs.each {|div| @divOrders[div-1] = 'allin'}
        when /otwier/i
          @divOrders = ['otwiera'] * 4
          divs = [1,2,3,4]
        when /zamyk.+min/i
          divs.each {|div| @divOrders[div-1] = 'zamyka'}
        when /zamyk/i
          @divOrders = [nil] * 4
      end
    end
    
    total = ordersString
    @battleSide = nil if total.empty?
    time = Time.now
    divs.each {|d| @gatherTimes[d] = time}
    @gatherTimes[0] = time
    
    YAML::DBM.open 'hdata' do |db|
      db['battleLink'] = @battleLink
      db['battleSide'] = @battleSide
      db['divOrders'] = @divOrders
      db['gatherTimes'] = @gatherTimes
    end
    
    massslap divs
    pafsay orderStr
    
    unless parseError
      pafsay "#{'eGov:'.bold} #{@egov}" if @divOrders.any?{|d| d == 'otwiera'}
      pafsay "#{'Razem:'.bold.brown.bgyellow} #{total}" unless total.empty?
    else pafsay "Nie rozpoznano rozkazu" end
  end
  
  def pafhandle msg, sender
    if msg.match /^\.(\S+)\s?(.*)$/

      cmd, params = $~[1], $~[2]
      
      # paf commands
      case cmd
        when 'bicie'
          if params.empty? || params.match(/^d([1-4])$/)
            div = $~[1].to_i
            orders = ordersString
            divtime = @gatherTimes[div].strftime "%H:%M" if div != 0
            
            if orders.empty?
              time = @gatherTimes[0].strftime "%H:%M"
              if div != 0
                strDiv = "Zbiórka dla div #{div} zakończyła się o #{divtime.bold}" 
                strLast = "(ostatnia zbiórka - #{time.bold})"
                pafsay "#{strDiv} #{strLast}"
                  
              else  pafsay "Ostatnia zbiórka zakończyła się o #{time.bold}" end
            elsif div != 0
              order = @divOrders[div-1]
              orders = ordersString div
              
              if orders.empty?
                strOrders = "Zbiórka trwa, ale brak rozkazów dla div #{div}" 
                strLast = "(ostatni o #{divtime.bold})" 
                pafsay  "#{strOrders} #{strLast}"
                
              else
                if order == 'allin' || order.is_a?(Integer)
                  pafsay "#{'Bitwa:'.bold} #{@battleLink}"
                end
                
                pafsay "#{'eGov:'.bold} #{@egov} -- #{@battleSide}" if order != 'zamyka'
                pafsay "#{'Razem'.bold} dla d#{div}: #{orders}"
              end
            else
              if @divOrders.any? {|d| d == 'allin' || d.is_a?(Integer)}
                pafsay "#{'Bitwa:'.bold} #{@battleLink}"
              end
              if @divOrders.any? {|d| d != 'zamyka' && !d.nil? }
                pafsay "#{'eGov:'.bold} #{@egov} -- #{@battleSide}"
              end
              
              pafsay "#{'Razem:'.bold.brown.bgyellow} #{orders}"
            end
          else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['bicie'][0]}" end
        when 's'
          if checkMode sender, '@', @pafchan
            if params.empty? then massslap
            elsif params.match /^(d[1-4](?:,\s?[1-4])*)\s?(.*)$/
              msg, divs = $~[2], $~[1].scan(/\d/).map{|div| div.to_i}
              massslap divs
              pafsay msg.bold.brown.bgyellow unless msg.empty?
            else
              massslap
              pafsay params.bold.brown.bgyellow
            end
          end
        when 'div'
          if params.match /^([1-4\-])$/
            for div in @divNames do div.delete sender end
            d = $~[1]
            @divNames[d.to_i-1].push sender unless d == '-'
            YAML::DBM.open('hdata') {|db| db['divNames'] = @divNames}
            
            if d == '-' then pafsay "Usunieto #{sender.bold} z listy"
            else pafsay "Dodano #{sender.bold} do div #{$~[1]}" end
          else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['div'][0]}" end
        when 'rozkaz'
          if checkMode sender, '@', @pafchan
            params.match /^(\S+) (\S+)$/
            divsStr, orderStr = $~.nil? ? [nil, nil] : [$~[1], $~[2]]
            orderrules = /(^-?\d+$|^allin$|^zamyka$|^otwiera$|^brak$)/
            
            divmatch = divsStr.nil? ? nil : divsStr.match(/^d[1-4](,\s?[1-4])*$/)
            ordermatch = orderStr.nil? ? nil : orderStr.match(orderrules)
            
            if divmatch && ordermatch
              order = $~[1]
              divs = divsStr.scan(/\d/).map{|div| div.to_i}
              textorders = %w{otwiera zamyka allin}
              
              if order == 'brak' then order = nil
              else order = order.to_i unless textorders.include?(order) end
              
              time = Time.now
              divs.each do |div|
                if @divOrders[div-1].is_a?(Integer) && order.is_a?(Integer)
                  @divOrders[div-1] += order
                else @divOrders[div-1] = order end
                
                @gatherTimes[div] = time
              end
              @gatherTimes[0] = time
              YAML::DBM.open('hdata') do |db|
                db['divOrders'] = @divOrders
                db['gatherTimes'] = @gatherTimes
              end
              
              pafsay "Ustawiono rozkaz"
            else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['rozkaz'][0]}" end
          end
        when 'razem'
          orders = ordersString
          if orders.empty?
            pafsay 'Brak rozkazów'
          else pafsay orders end
        when 'bitwa'
          if params.empty?
            if @divOrders.any? {|order| !order.nil?}
              pafsay "#{'Bitwa:'.bold} #{@battleLink} -- #{@battleSide}"
            else pafsay "Nie ma teraz bicia" end
          elsif checkMode sender, '@', @pafchan
            @battleLink = params
            YAML::DBM.open('hdata') {|db| db['battleLink'] = @battleLink}
            pafsay 'Ustawiono bitwę'
          end
        when 'strona'
          if params.empty?
            if @divOrders.any? {|order| !order.nil?}
              if @battleSide.nil?
                pafsay 'Bijemy po standardowej stronie'
              else pafsay "#{'Strona bitwy:'.bold} #{@battleSide}" end
            else pafsay "Nie ma teraz bicia" end
          elsif checkMode sender, '@', @pafchan
            @battleSide = params
            YAML::DBM.open('hdata') {|db| db['battleSide'] = @battleSide}
            pafsay 'Ustawiono stronę bitwy'
          end
        when 'dodaj'
          if checkMode sender, '@', @pafchan
            if params.match /^(\w+) (.+)$/
              cmd, text = $~[1], $~[2]
              
              if @cmdsDescr.has_key? cmd
                pafsay 'Nie można zmienić stałej komendy'
              else
                @customCmds[cmd] = text
                YAML::DBM.open('hdata') {|db| db['customCmds'] = @customCmds}
                pafsay "Ustawiono komendę '#{cmd.bold}'"
              end
            else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['dodaj'][0]}" end
          end
        when 'usun'
          if checkMode sender, '@', @pafchan
            if params.match /^(\w+)$/
              cmd = $~[1]
              
              if @cmdsDescr.has_key? cmd
                pafsay 'Nie można usunąć stałej komendy'
              elsif @customCmds.delete cmd
                YAML::DBM.open('hdata') {|db| db['customCmds'] = @customCmds}
                pafsay "Usunięto komendę '#{cmd.bold}'"
              else pafsay "Komenda '#{cmd.bold}' nie istnieje" end
            else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['usun'][0]}" end
          end
        when 'egov'
          if params.empty?
            pafsay "#{'eGov:'.bold} #{@egov}"
          elsif checkMode sender, '@', @pafchan
            @egov = params
            YAML::DBM.open('hdata') {|db| db['egov'] = @egov}
            pafsay 'Ustawiono link do egova'
          end
        when 'oblicz'
          if params =~ /^(sqrt|[\s\.,()0-9e^*\/+-])+$/
            expr = params.gsub(',','.').gsub('^','**').gsub('sqrt', 'Math.sqrt')
            expr.gsub! /([^.\d]|^)(\d+)([^.\d]|$)/, '\1\2.0\3'
            
            expr.gsub! /(e-?\d+)\.0/, '\1'
            
            begin
              res = (((eval expr)*10000).round.to_f / 10000.0).to_s
              
              parts = res.split '.'
              parts[0].gsub /(\d)(?=\d{3}+$)/, '\1 '
              
              res = parts.join('.').gsub /\.0+$/, ''
              
              pafsay res
            rescue SyntaxError
              pafsay 'Nieprawidłowe wyrażenie'
            rescue
              pafsay 'Nieprawidłowe wyrażenie'
            end
          else pafsay 'Nieprawidłowe wyrażenie' end
        when 'seen'
          if params.empty?
            pafsay "#{'Użycie:'.bold} #{@cmdsDescr['seen'][0]}"
          elsif params == 'Hamster[PAF]' then pafsay 'To ja!'
          elsif @names[@pafchan].any? {|name| name =~ /[@+]?#{Regexp.escape(params)}$/i }
            pafsay "#{params.bold} jest na kanale"
          else
            seenName = @seenNames[params.downcase]
            if seenName.nil?
              pafsay "Nie widziano #{params.bold}"
            else
              seenStr = "Ostatni raz widziano #{params.bold}" 
              if seenName[0] == 'leave'
                pafsay  "#{seenStr} opuszczającego kanał #{seenName[1].bold}"
              elsif seenName[0] == 'nickchange'
                pafsay "#{seenStr} zmieniającego nick na #{seenName[1].bold} #{seenName[2].bold}"
              end
            end
          end
        when 'rozkazymon'
          if params.empty?
            if @monOrders == 'on'
              pafsay 'Przekazywanie rozkazów MON włączone'
            else pafsay 'Przekazywanie rozkazów MON wyłączone' end
          elsif checkMode sender, '@', @pafchan
            if ['on', 'off'].include? params
              if params == 'on'
                if @monOrders == params
                  pafsay 'Przekazywanie rozkazów MON jest już włączone'
                else pafsay 'Włączono tryb przekazywania rozkazów MON' end
              else
                if @monOrders == params
                  pafsay 'Przekazywanie rozkazów MON jest już wyłączone'
                else pafsay 'Wyłączono tryb przekazywania rozkazów MON' end
              end
              @monOrders = params
              YAML::DBM.open('hdata') {|db| db['monOrders'] = @monOrders}
            else pafsay "#{'Użycie:'.bold} #{cmdsDescr['rozkazymon'][0]}" end
          end
        when 'davila'
          if params.empty?
            i = rand(@davila.count)
            num = "##{(i+1)}".bold
            pafsay "#{num} #{@davila[i]}"
          elsif params.match /#(\d{1,3})/
            i = $1.to_i
            if (1..@davila.count).include? i then pafsay @davila[i-1]
            else pafsay "#{'Użycie:'.bold} #{@cmdsDescr['davila'][0]}" end
          else
            quotes = @davila.select {|quote| quote.downcase.include? params.downcase}
            unless quotes.empty?
              quote = quotes[rand(quotes.length)]
              num = "##{(@davila.index(quote)+1)}".bold
              pafsay "#{num} #{quote}"
            else pafsay 'Nic nie znaleziono' end
          end
        when 'pomoc'
          if params.empty?
            commands = @cmdsDescr.keys.sort.join " | ".bold
            customCmds = @customCmds.keys.sort.join " | ".bold
            pafsay "#{'Komendy bota:'.bold} #{commands}"
            pafsay "#{'Dodatkowe komendy:'.bold} #{customCmds}"
            pafsay "Więcej informacji: .pomoc (komenda)"
          elsif @cmdsDescr.has_key? params
            cmd = @cmdsDescr[params]
            pafsay "#{cmd[0]} -- #{cmd[1]}"
            opt, req = "[ ] - cz. opcjonalna", "{ } - cz. wymagana"
            cust, div = "(treść) - wymagany ciąg znaków", "1-4 - nr diva"
            alt, rep = "| - alternatywa", "... - powtórzenia"
            pafsay "Objaśnienia: #{opt}; #{req}; #{cust}; #{div}; #{alt}; #{rep}"
          elsif @customCmds.has_key? params
            pafsay "Komenda dodatkowa; użycie: .#{params}"
          else pafsay "Nie ma takiej komendy" end
        when 'ver'
          pafsay "Hamster bot v.#{VER}"
        else
          text = @customCmds[cmd]
          pafsay text unless text.nil?
      end
    end
    
    if msg.match /(youtube\.com\/\S+)/
      begin
        site = Nokogiri::HTML(open("http://www.#{$~[1]}").read)
        title = site.css('#eow-title').text.strip
        youtube = "#{'You'.black.bglightgrey}#{'Tube'.white.bgred}".bold
        pafsay "#{youtube} #{title}"
      rescue
      end
    elsif msg.match /(^| )ale ja / then pafsay 'Milcz, nie protestuj!' end
  end
  
  def dowhandle msg, sender
puts "CLEANED DOWHANDLE"
puts msg.normal
puts
    return if @monOrders == 'off'
    return unless checkMode sender, '@+', @dowchan
    rules = [ /erepublik.com\/\w+\/(military|wars).+(hit?|all?\s*in)/i,
      /(otwiera|open).+(egov|d.*[1-4])/i, /d.*[1-4]\s+(otwiera|open)/i,
      /(zamyk|close).+(min|d.*[1-4])/i, /d.*[1-4].+(zamyk|close)/i,
      /(zamyka|close)\S*\s*(egov)?\s*(,.+|,?.+bicie\s*)?/i ]
    if rules.any? {|rule| msg =~ rule} then paforder msg end
  end

  def run
    loop do
      if @msgqueue.empty?
        break if @socket.eof?
        msg = @socket.gets.rstrip
        puts msg
      else msg = @msgqueue.shift end

      if msg.match /:(.+)!.+PRIVMSG #{@dowchan} :(.*)$/
        dowhandle $~[2], $~[1]
      elsif msg.match /:(.+)!.+PRIVMSG #{@pafchan} :(.*)$/
        pafhandle $~[2], $~[1]
      elsif msg.match /^PING :(.*)$/
        say "PONG #{$~[1]}"
        
      # actions for modifying the 'names' list
      elsif msg.match /:(.+)!.+ PART (\S+)/
        user, channel = $~[1], $~[2]
        regex = /[@+]?#{Regexp.escape(user)}$/
        @names[channel].delete_if {|name| name.match regex }
        if user == 'Hamster[PAF]'
          if channel == @pafchan then join @pafchan
          else join @dowchan, @dowpass end
        end
        date = Time.now.strftime('%d.%m.%y o %H:%M')
        @seenNames[user.downcase] = ['leave', date]
        YAML::DBM.open('hdata') {|db| db['seenNames'] = @seenNames}
      elsif msg.match /KICK (\S+) (\S+)/
        user, channel = $~[2], $~[1]
        regex = /[@+]?#{Regexp.escape(user)}$/
        @names[channel].delete_if {|name| name.match regex }
        if user == 'Hamster[PAF]'
          if channel == @pafchan then join @pafchan
          else join @dowchan, @dowpass end
        end
        date = Time.now.strftime('%d.%m.%y o %H:%M')
        @seenNames[user.downcase] = ['leave', date]
        YAML::DBM.open('hdata') {|db| db['seenNames'] = @seenNames}
      elsif msg.match /:(.+)!.+ QUIT/
        user = $~[1]
        @names.each_value do |names|
          names.delete_if {|name| name.match /[@+]?#{Regexp.escape(user)}$/ }
        end
        date = Time.now.strftime('%d.%m.%y o %H:%M')
        @seenNames[user.downcase] = ['leave', date]
        YAML::DBM.open('hdata') {|db| db['seenNames'] = @seenNames}
      elsif msg.match /:(.+)!.+ JOIN (.+)/
        @names[$~[2]].push $~[1]
      elsif msg.match /:(.+)!.+ NICK :(.+)$/
        oldNick, newNick = $~[1], $~[2].rstrip
        @names.each_value do |channel|
          pos = channel.index{|n| n.match /([@+]?)#{Regexp.escape(oldNick)}$/ }
          channel[pos] = "#{$~[1]}#{newNick}" unless pos.nil?
        end
        date = Time.now.strftime('%d.%m.%y o %H:%M')
        @seenNames[oldNick.downcase] = ['nickchange', newNick, date]
        YAML::DBM.open('hdata') {|db| db['seenNames'] = @seenNames}
      elsif msg.match /MODE (\S+) ([\w+-]+) (.+)$/

        chan, options, givenNames = $~[1], $~[2], $~[3].split(' ')
        i, opt = 0, ''

        options.each_char do |o|
          if o == '+' || o == '-'
            opt = o
          elsif o == 'o' || o == 'v'
            pos = @names[chan].index{|n| n =~ /[@+]?#{Regexp.escape(givenNames[i])}$/ }
            if opt == '+'
              @names[chan][pos] = "@#{givenNames[i]}" if o == 'o'
              @names[chan][pos] = "+#{givenNames[i]}" if o == 'v'
            else @names[chan][pos] = givenNames[i] end
            i += 1
          else i+= 1 end
        end
      end
    end
  end
  
end

loop do
  hamsterBot = Hamster.new('irc.quakenet.org', 6667)
  hamsterBot.run
  sleep 7
end
