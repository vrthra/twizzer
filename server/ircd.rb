module NetUtils
    def carp(arg)
        if $verbose
            case  true
            when arg.kind_of?(Exception)
                puts "Error:" + arg.message 
                #puts "#{self.class.to_s.downcase}:" + arg.message 
                #puts arg.backtrace.collect{|s| "#{self.class.to_s.downcase}:" + s}.join("\n")
            else
                puts "#{self.class.to_s.downcase}:" + arg
            end
        end
    end

    def get_resource( resource )
        $:.each do |lp|
            lp << '/' unless lp =~ /\/$/
            res = "#{lp}#{resource}.rb"
            if lp =~ /http:\/\//i
                begin
                    response = Net::HTTP.get_response(URI.parse(res))
                    if response.code.to_i == 200
                        return response.body
                    else
                        raise response.code
                    end
                rescue Exception => e
                    raise e.message()
                end
            else
                return File.read(res) if FileTest.exist?(res)
            end
        end
    end
end
module IRCReplies
RPL_WELCOME		=		001
RPL_YOURHOST		=		002
RPL_CREATED		=		003
RPL_MYINFO		=		004
RPL_BOUNCE		=		005
RPL_TRACELINK		=		200
RPL_TRACECONNECTING		=		201
RPL_TRACEHANDSHAKE		=		202
RPL_TRACEUNKNOWN		=		203
RPL_TRACEOPERATOR		=		204
RPL_TRACEUSER		=		205
RPL_TRACESERVER		=		206
RPL_TRACESERVICE		=		207
RPL_TRACENEWTYPE		=		208
RPL_TRACECLASS		=		209
RPL_TRACERECONNECT		=		210
RPL_TRACELOG		=		261
RPL_TRACEEND		=		262
RPL_STATSLINKINFO		=		211
RPL_STATSCOMMANDS		=		212
RPL_ENDOFSTATS		=		219
RPL_STATSUPTIME		=		242
RPL_STATSOLINE		=		243
RPL_UMODEIS		=		221
RPL_SERVLIST		=		234
RPL_SERVLISTEND		=		235
RPL_LUSERCLIENT		=		251
RPL_LUSEROP		=		252
RPL_LUSERUNKNOWN		=		253
RPL_LUSERCHANNELS		=		254
RPL_LUSERME		=		255
RPL_ADMINME		=		256
RPL_ADMINEMAIL		=		259
RPL_TRYAGAIN		=		263
RPL_USERHOST		=		302
RPL_ISON		=		303
RPL_AWAY		=		301
RPL_UNAWAY		=		305
RPL_NOWAWAY		=		306
RPL_WHOISUSER		=		311
RPL_WHOISSERVER		=		312
RPL_WHOISOPERATOR		=		313
RPL_WHOISIDLE		=		317
RPL_ENDOFWHOIS		=		318
RPL_WHOISCHANNELS		=		319
RPL_WHOWASUSER		=		314
RPL_ENDOFWHOWAS		=		369
RPL_LISTSTART		=		321
RPL_LIST		=		322
RPL_LISTEND		=		323
RPL_UNIQOPIS		=		325
RPL_CHANNELMODEIS		=		324
RPL_NOTOPIC		=		331
RPL_TOPIC		=		332
RPL_INVITING		=		341
RPL_SUMMONING		=		342
RPL_INVITELIST		=		346
RPL_ENDOFINVITELIST		=		347
RPL_EXCEPTLIST		=		348
RPL_ENDOFEXCEPTLIST		=		349
RPL_VERSION		=		351
RPL_WHOREPLY		=		352
RPL_ENDOFWHO		=		315
RPL_NAMREPLY		=		353
RPL_ENDOFNAMES		=		366
RPL_LINKS		=		364
RPL_ENDOFLINKS		=		365
RPL_BANLIST		=		367
RPL_ENDOFBANLIST		=		368
RPL_INFO		=		371
RPL_ENDOFINFO		=		374
RPL_MOTDSTART		=		375
RPL_MOTD		=		372
RPL_ENDOFMOTD		=		376
RPL_YOUREOPER		=		381
RPL_REHASHING		=		382
RPL_YOURESERVICE		=		383
RPL_TIME		=		391
RPL_USERSSTART		=		392
RPL_USERS		=		393
RPL_ENDOFUSERS		=		394
RPL_NOUSERS		=		395
ERR_NOSUCHNICK		=		401
ERR_NOSUCHSERVER		=		402
ERR_NOSUCHCHANNEL		=		403
ERR_CANNOTSENDTOCHAN		=		404
ERR_TOOMANYCHANNELS		=		405
ERR_WASNOSUCHNICK		=		406
ERR_TOOMANYTARGETS		=		407
ERR_NOSUCHSERVICE		=		408
ERR_NOORIGIN		=		409
ERR_NORECIPIENT		=		411
ERR_NOTEXTTOSEND		=		412
ERR_NOTOPLEVEL		=		413
ERR_WILDTOPLEVEL		=		414
ERR_BADMASK		=		415
ERR_UNKNOWNCOMMAND		=		421
ERR_NOMOTD		=		422
ERR_NOADMININFO		=		423
ERR_FILEERROR		=		424
ERR_NONICKNAMEGIVEN		=		431
ERR_ERRONEUSNICKNAME		=		432
ERR_NICKNAMEINUSE		=		433
ERR_NICKCOLLISION		=		436
ERR_UNAVAILRESOURCE		=		437
ERR_USERNOTINCHANNEL		=		441
ERR_NOTONCHANNEL		=		442
ERR_USERONCHANNEL		=		443
ERR_NOLOGIN		=		444
ERR_SUMMONDISABLED		=		445
ERR_USERSDISABLED		=		446
ERR_NOTREGISTERED		=		451
ERR_NEEDMOREPARAMS		=		461
ERR_ALREADYREGISTRED		=		462
ERR_NOPERMFORHOST		=		463
ERR_PASSWDMISMATCH		=		464
ERR_YOUREBANNEDCREEP		=		465
ERR_YOUWILLBEBANNED		=		466
ERR_KEYSET		=		467
ERR_CHANNELISFULL		=		471
ERR_UNKNOWNMODE		=		472
ERR_INVITEONLYCHAN		=		473
ERR_BANNEDFROMCHAN		=		474
ERR_BADCHANNELKEY		=		475
ERR_BADCHANMASK		=		476
ERR_NOCHANMODES		=		477
ERR_BANLISTFULL		=		478
ERR_NOPRIVILEGES		=		481
ERR_CHANOPRIVSNEEDED		=		482
ERR_CANTKILLSERVER		=		483
ERR_RESTRICTED		=		484
ERR_UNIQOPPRIVSNEEDED		=		485
ERR_NOOPERHOST		=		491
ERR_UMODEUNKNOWNFLAG		=		501
ERR_USERSDONTMATCH		=		502
end
require 'webrick'
require 'thread'

include IRCReplies

$config ||= {}
$config['version'] = '0.04dev'
$config['timeout'] = 10
$config['port'] = 6667
$config['hostname'] = Socket.gethostname.split(/\./).shift
$config['starttime'] = Time.now.to_s
$config['nick-tries'] = 5

$verbose = ARGV.shift || false
    
CHANNEL = /^[#\$&]+/
PREFIX  = /^:[^ ]+ +(.+)$/

class SynchronizedStore
    def initialize
        @store = {}
        @mutex = Mutex.new
    end
    
    def method_missing(name,*args)
        @mutex.synchronize { @store.__send__(name,*args) }
    end

    def each_value
        @mutex.synchronize do
            @store.each_value {|u|
                @mutex.unlock
                yield u
                @mutex.lock
            }
        end
    end

    def keys
        @mutex.synchronize{@store.keys}
    end
end

$user_store = SynchronizedStore.new
class << $user_store
    def <<(client)
        self[client.nick] = client
    end
    
    alias nicks keys
    alias each_user each_value 
end

$channel_store = SynchronizedStore.new
class << $channel_store
    def add(c)
        self[c] ||= IRCChannel.new(c)
    end

    def remove(c)
        self.delete[c]
    end
    
    alias each_channel each_value 
    alias channels keys
end

class IRCChannel < SynchronizedStore
    include NetUtils
    attr_reader :name, :topic
    alias each_user each_value 

    def initialize(name)
        super()

        @topic = "There is no topic"
        @name = name
        @oper = []
        carp "create channel:#{@name}"
    end

    def add(client)
        @oper << client.nick if @oper.empty? and @store.empty?
        self[client.nick] = client
    end
    
    def remove(client)
        delete(client.nick)
    end

    def join(client)
        return false if is_member? client
        add client
        #send join to each user in the channel
        each_user {|user|
            user.reply :join, client.userprefix, @name
        }
        return true
    end

    def part(client, msg)
        return false if !is_member? client
        each_user {|user|
            user.reply :part, client.userprefix, @name, msg
        }
        remove client
        $channel_store.delete(@name) if self.empty?
        return true
    end

    def quit(client, msg)
        #remove client should happen before sending notification
        #to others since we dont want a notification to ourselves
        #after quit.
        remove client
        each_user {|user|
            user.reply :quit, client.userprefix, @name, msg if user!= client
        }
        $channel_store.delete(@name) if self.empty?
    end

    def privatemsg(msg, client)
        each_user {|user|
            user.reply :privmsg, client.userprefix, @name, msg if user != client
        }
    end

    def notice(msg, client)
        each_user {|user|
            user.reply :notice, client.userprefix, @name, msg if user != client
        }
    end

    def topic(msg=nil,client=nil)
        return @topic if msg.nil?
        @topic = msg
        each_user {|user|
            user.reply :topic, client.userprefix, @name, msg
        }
        return @topic
    end

    def nicks
        return keys
    end

    def mode(u)
        return @oper.include?(u.nick) ? '@' : ''
    end

    def is_member?(m)
        values.include?(m)
    end

    alias has_nick? is_member?
end

class IRCClient
    include NetUtils

    attr_reader :nick, :user, :realname, :channels, :state

    def initialize(sock, serv)
        @serv = serv
        @socket = sock
        @channels = []
        @peername = peer()
        @welcomed = false
        @nick_tries = 0
        @state = {}
        carp "initializing connection from #{@peername}"
    end

    def host
        return @peername
    end

    def userprefix
        return @usermsg
    end

    def closed?
        return @socket.nil? || @socket.closed?
    end

    def ready
        #check for nick and pass
        return (!@pass.nil? && !@nick.nil?) ? true : false
    end

    def peer
        begin
            sockaddr = @socket.getpeername
            begin
                return Socket.getnameinfo(sockaddr, Socket::NI_NAMEREQD).first
            rescue 
                return Socket.getnameinfo(sockaddr).first
            end
        rescue
            return @socket.peeraddr[2]
        end
    end

    def handle_pass(s)
        carp "pass = #{s}"
        @pass = s
    end
    
    def handle_nick(s)
        carp "nick => #{s}"
        if $user_store[s].nil?
            userlist = {}
            if @nick.nil?
                handle_newconnect(s)
            else
                userlist[s] = self if self.nick != s
                $user_store.delete(@nick)
                @nick = s
            end

            $user_store << self

            #send the info to the world
            #get unique users.
            @channels.each {|c|
                $channel_store[c].each_user {|u|
                    userlist[u.nick] = u
                }
            }
            userlist.values.each {|user|
                user.reply :nick, s
            }
            @usermsg = ":#{@nick}!~#{@user}@#{@peername}"
        else
            #check if we are just nicking ourselves.
            unless $user_store[s] == self
                #verify the connectivity of earlier guy
                unless $user_store[s].closed?
                    reply :numeric, ERR_NICKNAMEINUSE,"* #{s} ","Nickname is already in use."
                    @nick_tries += 1
                    if @nick_tries > $config['nick-tries']
                        carp "kicking spurious user #{s} after #{@nick_tries} tries"
                        handle_abort
                    end
                    return
                else
                    $user_store[s].handle_abort
                    $user_store[s] = self
                end
            end
        end
        @nick_tries = 0
    end

    def handle_user(user, mode, unused, realname)
        @user = user
        @mode = mode
        @realname = realname
        @usermsg = ":#{@nick}!~#{@user}@#{@peername}"
        send_welcome if !@nick.nil?
    end

    def mode
        return @mode
    end

    def handle_newconnect(nick)
        @alive = true
        @nick = nick
        @host = $config['hostname']
        @ver = $config['version']
        @starttime = $config['starttime']
        send_welcome if !@user.nil?
    end

    def send_welcome
        if !@welcomed
            repl_welcome
            repl_yourhost
            repl_created
            repl_myinfo
            repl_motd
            repl_mode
            @welcomed = true
        end
    end

    def repl_welcome
        client = "#{@nick}!#{@user}@#{@peername}"
        reply :numeric, RPL_WELCOME, @nick, "Welcome to this IRC server #{client}"
    end

    def repl_yourhost
        reply :numeric, RPL_YOURHOST, @nick, "Your host is #{@host}, running version #{@ver}"
    end

    def repl_created
        reply :numeric, RPL_CREATED, @nick, "This server was created #{@starttime}"
    end

    def repl_myinfo
        reply :numeric, RPL_MYINFO, @nick, "#{@host} #{@ver} #{@serv.usermodes} #{@serv.channelmodes}"
    end

    def repl_bounce(sever, port)
        reply :numeric, RPL_BOUNCE ,"Try server #{server}, port #{port}"
    end

    def repl_ison()
        #XXX TODO
        reply :numeric, RPL_ISON,"notimpl"
    end

    def repl_away(nick, msg)
        reply :numeric, RPL_AWAY, nick, msg
    end

    def repl_unaway()
        reply :numeric, RPL_UNAWAY, @nick,"You are no longer marked as being away"
    end

    def repl_nowaway()
        reply :numeric, RPL_NOWAWAY, @nick,"You have been marked as being away"
    end

    def repl_motd()
        reply :numeric, RPL_MOTDSTART,'', "- Message of the Day"
        reply :numeric, RPL_MOTD,'',      "- Do the dance see the source"
        reply :numeric, RPL_ENDOFMOTD,'', "- End of /MOTD command."
    end

    def repl_mode()
    end


    def send_nonick(nick)
        reply :numeric, ERR_NOSUCHNICK, nick, "No such nick/channel"
    end

    def send_nochannel(channel)
        reply :numeric, ERR_NOSUCHCHANNEL, channel, "That channel doesn't exist"
    end

    def send_notonchannel(channel)
        reply :numeric, ERR_NOTONCHANNEL, channel, "Not a member of that channel"
    end

    def send_topic(channel)
        if $channel_store[channel]
            reply :numeric, RPL_TOPIC,channel, "#{$channel_store[channel].topic}" 
        else
            send_notonchannel channel
        end
    end

    def names(channel)
        return $channel_store[channel].nicks
    end

    def send_nameslist(channel)
        c =  $channel_store[channel]
        if c.nil?
            carp "names failed :#{c}"
            return 
        end
        names = []
        c.each_user {|user|
            names << c.mode(user) + user.nick if user.nick
        }
        reply :numeric, RPL_NAMREPLY,"= #{c.name}","#{names.join(' ')}"
        reply :numeric, RPL_ENDOFNAMES,"#{c.name} ","End of /NAMES list."
    end

    def send_ping()
        reply :ping, "#{$config['hostname']}"
    end

    def handle_join(channels)
        channels.split(/,/).each {|ch|
            c = ch.strip
            if c !~ CHANNEL
                send_nochannel(c)
                carp "no such channel:#{c}"
                return
            end
            channel = $channel_store.add(c)
            if channel.join(self)
                send_topic(c)
                send_nameslist(c)
                @channels << c
            else
                carp "already joined #{c}"
            end
        }
    end

    def handle_ping(pingmsg, rest)
        reply :pong, pingmsg
    end

    def handle_pong(srv)
        carp "got pong: #{srv}"
    end

    def handle_privmsg(target, msg)
        case target.strip
        when CHANNEL
            channel= $channel_store[target]
            if !channel.nil?
                channel.privatemsg(msg, self)
            else
                send_nonick(target)
            end
        else
            user = $user_store[target]
            if !user.nil?
                if !user.state[:away].nil?
                    repl_away(user.nick,user.state[:away])
                end
                user.reply :privmsg, self.userprefix, user.nick, msg
            else
                send_nonick(target)
            end
        end
    end

    def handle_notice(target, msg)
        case target.strip
        when CHANNEL
            channel= $channel_store[target]
            if !channel.nil?
                channel.notice(msg, self)
            else
                send_nonick(target)
            end
        else
            user = $user_store[target]
            if !user.nil?
                user.reply :notice, self.userprefix, user.nick, msg
            else
                send_nonick(target)
            end
        end
    end

    def handle_part(channel, msg)
        if $channel_store.channels.include? channel
            if $channel_store[channel].part(self, msg)
                @channels.delete(channel)
            else
                send_notonchannel channel
            end
        else
            send_nochannel channel
        end
    end

    def handle_quit(msg)
        #do this to avoid double quit due to 2 threads.
        return if !@alive
        @alive = false
        @channels.each do |channel|
            $channel_store[channel].quit(self, msg)
        end
        $user_store.delete(self.nick)
        carp "#{self.nick} #{msg}"
        @socket.close if !@socket.closed?
    end

    def handle_topic(channel, topic)
        carp "handle topic for #{channel}:#{topic}"
        if topic.nil? or topic =~ /^ *$/
            send_topic(channel)
        else
            begin
                $channel_store[channel].topic(topic,self)
            rescue Exception => e
                carp e
            end
        end
    end

    def handle_away(msg)
        carp "handle away :#{msg}"
        if msg.nil? or msg =~ /^ *$/
            @state.delete(:away)
            repl_unaway
        else
            @state[:away] = msg
            repl_nowaway
        end
    end
        
    def handle_list(channel)
        reply :numeric, RPL_LISTSTART
        case channel.strip
        when /^#/
            channel.split(/,/).each {|cname|
                c = $channel_store[cname.strip]
                reply :numeric, RPL_LIST, c.name, c.topic if c
            }
        else
            #older opera client sends LIST <1000
            #we wont obey the boolean after list, but allow the listing
            #nonetheless
            $channel_store.each_channel {|c|
                reply :numeric, RPL_LIST, c.name, c.topic
            }
        end
        reply :numeric, RPL_LISTEND
    end

    def handle_whois(target,nicks)
        #ignore target for now.
        return reply(:numeric, RPL_NONICKNAMEGIVEN, "", "No nickname given") if nicks.strip.length == 0
        nicks.split(/,/).each {|nick|
            nick.strip!
            user = $user_store[nick]
            if user
                reply :numeric, RPL_WHOISUSER, "#{user.nick} #{user.user} #{user.host} *", "#{user.realname}"
                reply :numeric, RPL_WHOISCHANNELS, user.nick, "#{user.channels.join(' ')}"
                repl_away user.nick, user.state[:away] if !user.state[:away].nil?
                reply :numeric, RPL_ENDOFWHOIS, user.nick, "End of /WHOIS list"
            else
                return send_nonick(nick) 
            end
        }
    end

    def handle_names(channels, server)
        channels.split(/,/).each {|ch| send_nameslist(ch.strip) }
    end

    def handle_who(mask, rest)
        channel = $channel_store[mask]
        hopcount = 0
        if channel.nil?
            #match against all users
            $user_store.each_user {|user|
                reply :numeric, RPL_WHOREPLY ,
                    "#{user.channels[0]} #{user.userprefix} #{user.host} #{$config['hostname']} #{user.nick} H" , 
                    "#{hopcount} #{user.realname}" if File.fnmatch?(mask, "#{user.host}.#{user.realname}.#{user.nick}")
            }
            reply :numeric, RPL_ENDOFWHO, mask, "End of /WHO list."
        else
            #get all users in the channel
            channel.each_user {|user|
                reply :numeric, RPL_WHOREPLY ,
                    "#{mask} #{user.userprefix} #{user.host} #{$config['hostname']} #{user.nick} H" , 
                    "#{hopcount} #{user.realname}"
            }
            reply :numeric, RPL_ENDOFWHO, mask, "End of /WHO list."
        end
    end

    def handle_mode(target, rest)
        #TODO: dummy
        reply :mode, target, rest
    end

    def handle_userhost(nicks)
        info = []
        nicks.split(/,/).each {|nick|
            user = $user_store[nick]
            info << user.nick + '=-' + user.nick + '@' + user.peer
        }
        reply :numeric, RPL_USERHOST,"", info.join(' ')
    end

    def handle_reload(password)
    end

    def handle_abort()
        handle_quit('aborted..')
    end
        
    def handle_version()
        reply :numeric, RPL_VERSION,"#{$config['version']} Ruby IRCD", ""
    end
    
    def handle_eval(s)
        reply :raw, eval(s)
    end

    def handle_unknown(s)
        carp "unknown:>#{s}<"
        reply :numeric, ERR_UNKNOWNCOMMAND,s, "Unknown command"
    end

    def handle_connect
        reply :raw, "NOTICE AUTH :#{$config['version']} initialized, welcome."
    end
    
    def reply(method, *args)
        case method
        when :raw
            arg = *args
            raw arg
        when :ping
            host = *args
            raw "PING :#{host}"
        when :pong
            msg = *args
            # according to rfc 2812 the PONG must be of
            #PONG csd.bu.edu tolsun.oulu.fi
            # PONG message from csd.bu.edu to tolsun.oulu.fi
            # ie no host at the begining
            raw "PONG #{@host} #{@peername} :#{msg}"
        when :join
            user,channel = args
            raw "#{user} JOIN :#{channel}"
        when :part
            user,channel,msg = args
            raw "#{user} PART #{channel} :#{msg}"
        when :quit
            user,msg = args
            raw "#{user} QUIT :#{msg}"
        when :privmsg
            usermsg, channel, msg = args
            raw "#{usermsg} PRIVMSG #{channel} :#{msg}"
        when :notice
            usermsg, channel, msg = args
            raw "#{usermsg} NOTICE #{channel} :#{msg}"
        when :topic
            usermsg, channel, msg = args
            raw "#{usermsg} TOPIC #{channel} :#{msg}"
        when :nick
            nick = *args
            raw "#{@usermsg} NICK :#{nick}"
        when :mode
            nick, rest = args
            raw "#{@usermsg} MODE #{nick} :#{rest}"
        when :numeric
            numeric,msg,detail = args
            server = $config['hostname']
            raw ":#{server} #{'%03d'%numeric} #{@nick} #{msg} :#{detail}"
        end
    end
    
    def raw(arg, abrt=false)
        begin
        carp "--> #{arg}"
        @socket.print arg.chomp + "\n" if !arg.nil?
        rescue Exception => e
            carp "<#{self.userprefix}>#{e.message}"
            #puts e.backtrace.join("\n")
            handle_abort()
            raise e if abrt
        end
    end
end

class ProxyClient < IRCClient

    def initialize(nick, actor, serv)
        carp "Initializing service #{nick}"
        @nick = nick
        super(nil,serv)
        @conn = IrcClient::ProxyConnector.new(nick,'pass',self,actor)
    end

    def peer
        return @nick
    end

    def handle_connect
        @conn.connect
    end

    def getnick(user)
        if user =~ /^:([^!]+)!.*/
            return $1
        else
            return user
        end
    end

    def reply(method, *args)
        case method
        when :raw
            arg = *args
            @conn.invoke :unknown, arg
        when :ping
            host = *args
            @conn.invoke :ping, host
        when :pong
            msg = *args
            @conn.invoke :pong, msg
        when :join
            user,channel = args
            @conn.invoke :join, getnick(user), channel
        when :privmsg
            user, channel, msg = args
            @conn.invoke :privmsg, getnick(user), channel, msg
        when :notice
            user, channel, msg = args
            @conn.invoke :notice, getnick(user), channel, msg
        when :topic
            user, channel, msg = args
            @conn.invoke :topic, getnick(user), channel, msg
        when :nick
            nick = *args
            @conn.invoke :nick, nick
        when :mode
            nick, rest = args
            @conn.invoke :mode, nick, rest
        when :numeric
            numeric,msg,detail = args
            server = $config['hostname']
            @conn.invoke :numeric, server, numeric, msg, detail
        end
    end

    #From the local services
    def invoke(method, *args)
        case method
        when :pong
            server = *args
            handle_pong server
        when :pass
            pass = *args
            handle_pass pass
        when :nick
            nick = *args
            handle_nick nick
        when :user
            user, mode, vhost, realname = args
            handle_user user, mode, vhost, realname
        when :names
            channel, serv = *args
            handle_names channel, serv
        when :join
            channel = *args
            handle_join channel
        when :part
            channel, msg = args
            handle_part channel, msg
        when :quit
            msg = args
            handle_quit msg
        when :privmsg
            channel, msg = args
            handle_privmsg channel, msg
        else
            handle_unknown "#{method} #{args.join(',')}"
        end
    end
end

class IRCServer < WEBrick::GenericServer
    include NetUtils
    def usermodes
        return "aAbBcCdDeEfFGhHiIjkKlLmMnNopPQrRsStUvVwWxXyYzZ0123459*@"
    end

    def channelmodes
        return "bcdefFhiIklmnoPqstv"
    end

    def run(sock)
        client = IRCClient.new(sock, self)
        client.handle_connect
        irc_listen(sock, client)
    end

    def addservice(nick,actor)
        carp "Add service #{nick}"
        client = ProxyClient.new(nick, actor, self)
        client.handle_connect
        #the client is able to call the methods directly
        #so we dont need to bother about looping here.
    end

    def hostname
        begin
            sockaddr = @socket.getsockname
            begin
                return Socket.getnameinfo(sockaddr, Socket::NI_NAMEREQD).first
            rescue 
                return Socket.getnameinfo(sockaddr).first
            end
        rescue
            return @socket.peeraddr[2]
        end
    end

    def irc_listen(sock, client)
        begin
            while !sock.closed? && !sock.eof?
                s = sock.gets
                handle_client_input(s.chomp, client)
            end
        rescue Exception => e
            carp e
        end
        client.handle_abort()
    end

    def handle_client_input(input, client)
        carp "<-- #{input}"
        s = if input =~ PREFIX
                $1
            else
                input
            end
        case s
        when /^[ ]*$/
            return
        when /^PASS +(.+)$/i
            client.handle_pass($1.strip)
        when /^NICK +(.+)$/i
            client.handle_nick($1.strip) #done
        when /^USER +([^ ]+) +([0-9]+) +([^ ]+) +:(.*)$/i
            client.handle_user($1, $2, $3, $4) #done
        when /^USER +([^ ]+) +([0-9]+) +([^ ]+) +:*(.*)$/i
            #opera does this.
            client.handle_user($1, $2, $3, $4) #done
        when /^USER ([^ ]+) +[^:]*:(.*)/i
            #chatzilla does this.
            client.handle_user($1, '', '', $3) #done
        when /^JOIN +(.+)$/i
            client.handle_join($1) #done
        when /^PING +([^ ]+) *(.*)$/i
            client.handle_ping($1, $2) #done
        when /^PONG +:(.+)$/i , /^PONG +(.+)$/i
            client.handle_pong($1)
        when /^PRIVMSG +([^ ]+) +:(.*)$/i
            client.handle_privmsg($1, $2) #done
        when /^NOTICE +([^ ]+) +(.*)$/i
            client.handle_notice($1, $2) #done
        when /^PART :+([^ ]+) *(.*)$/i  
            #some clients require this.
            client.handle_part($1, $2) #done
        when /^PART +([^ ]+) *(.*)$/i
            client.handle_part($1, $2) #done
        when /^QUIT :(.*)$/i
            client.handle_quit($1) #done
        when /^QUIT *(.*)$/i
            client.handle_quit($1) #done
        when /^TOPIC +([^ ]+) *:*(.*)$/i
            client.handle_topic($1, $2) #done
        when /^AWAY +:(.*)$/i
            client.handle_away($1)
        when /^AWAY +(.*)$/i #for opera
            client.handle_away($1)
        when /^:*([^ ])* *AWAY *$/i
            client.handle_away(nil)
        when /^LIST *(.*)$/i
            client.handle_list($1)
        when /^WHOIS +([^ ]+) +(.+)$/i
            client.handle_whois($1,$2)
        when /^WHOIS +([^ ]+)$/i
            client.handle_whois(nil,$1)
        when /^WHO +([^ ]+) *(.*)$/i
            client.handle_who($1, $2)
        when /^NAMES +([^ ]+) *(.*)$/i
            client.handle_names($1, $2)
        when /^MODE +([^ ]+) *(.*)$/i
            client.handle_mode($1, $2)
        when /^USERHOST +:(.+)$/i
            #besirc does this (not accourding to RFC 2812)
            client.handle_userhost($1)
        when /^USERHOST +(.+)$/i
            client.handle_userhost($1)
        when /^RELOAD +(.+)$/i
            client.handle_reload($1)
        when /^VERSION *$/i
            client.handle_version()
        when /^EVAL (.*)$/i
            #strictly for debug
            client.handle_eval($1)
        else
            client.handle_unknown(s)
        end
    end

    def do_ping()
        while true
            sleep 60
            $user_store.each_user {|client|
                client.send_ping
            }
        end
    end
end


if __FILE__ == $0
    #require 'ircclient'
    s = IRCServer.new( :Port => $config['port'] )
    begin
        while arg = ARGV.shift
            case arg
            when /-v/
                $verbose = true
            end
        end
        trap("INT"){ 
            s.carp "killing #{$$}"
            system("kill -9 #{$$}")
            s.shutdown
        }
        p = Thread.new {
            s.do_ping()
        }
        
        #s.addservice('serviceclient',IrcClient::TestActor)
        s.start

        #p.join
    rescue Exception => e
        s.carp e
    end
end
