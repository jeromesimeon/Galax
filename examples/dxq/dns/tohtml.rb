#!/usr/bin/ruby -w

require 'ftools'

@@HTMLDIR=ARGV[0]
@@TMPFILE="/tmp/tohtml.html"

def xhtml2html(fname)
  fin = File.new(@@HTMLDIR+"/"+fname)
  File.open(@@TMPFILE, "w") { |fres|
    fin.each_line {|line| 
      line = line.gsub(/\.xhtml/, '.html')
      fres.puts line
    }
  }
  fin.close()
  newFname = fname.sub(/\.xhtml\z/, ".html")
  %x{mv #{@@TMPFILE} #{@@HTMLDIR}/#{newFname}}
end

Dir.foreach(@@HTMLDIR) do |file|
  if file =~ /\.xhtml\z/ 
    xhtml2html file    
    %x{rm #{@@HTMLDIR}/#{file}}
  elsif file =~ /\.html\z/
    xhtml2html file    
  end
end


