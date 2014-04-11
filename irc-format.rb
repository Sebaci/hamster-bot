# encoding: UTF-8
$KCODE = "UTF-8"

=begin
codes:
\x03c1,c2 - foreground and background colors (background optional) from 0 to 15
\x02 - bolc
\x16 - italic
\x1F - underline
\x0F - normal
=end

class String
  %w{white black blue green red brown purple olive yellow lightgreen
    teal cyan lightblue pink grey lightgrey}.each_with_index do |color, index|
    i = index.to_s.sub /^(\d)$/, '0\1'
    class_eval do
      define_method color do
        newStr = self.sub /\x0F$/, ''
        newStr.gsub! /\x03\d{1,2}(,\d{1,2})?/, "\x03#{i}\\1"
        newStr.gsub! /(^|\x0F)(?!(\x02|\x16|\x0F|\x1F)*\x03)/i, "\\1\x03#{i}"
        newStr + "\x0F"
      end
      
      define_method "bg#{color}" do
        newStr = self.sub /\x0F$/, ''
        newStr.gsub! /\x03(\d{1,2})(?:,\d{1,2})?/, "\x03\\1,#{i}"
        newStr.gsub! /(^|\x0F)(?!(\x02|\x16|\x0F|\x1F)*\x03)/i, "\\1\x0301,#{i}"
        newStr + "\x0F"
      end
    end
  end
  
  {:bold => "\x02", :italic => "\x16", :underline => "\x1F"}.each do |style, c|
    class_eval do
      define_method style do
        newStr = self.sub /\x0F$/, ''
        reg = /(^|\x0F)(?!(\x02|\x16|\x0F|\x1F|\x03\d{1,2}(,\d{1,2})?)*#{c})/
        newStr.gsub! reg, "\\1#{c}"
        newStr + "\x0F"
      end
    end
  end
  
  def normal
    self.gsub /(\x02|\x16|\x0F|\x1F|\x03\d{1,2}(,\d{1,2})?)/, ''
  end
  
  alias_method :reversed, :italic

end
