---
title: time_ago_in_words Monkey Patch
---

Today my friend, Bryan, gave me a great idea for a Rails monkey patch. It goes
something like this:

<!--more-->

``` {.ruby}
module ActionView::Helpers::DateHelper
  def time_ago_in_words(*args)
    [
      "Best of times",
      "Worst of times",
      "Time is money.",
      "Lost time is never found again.",
      "Time is but the stream I go a-fishing in.",
      "How did it get so late so soon?",
      "Time flies like an arrow; fruit flies like a banana."
    ].sample
  end
end
 
# $ irb
# >> module ActionView; end
# => nil
# >> module ActionView::Helpers; end
# => nil
# >> load 'date_helper.rb'
# => true
# >> a = Object.new.extend ActionView::Helpers::DateHelper
# => #<Object:0x007f9bd41001e0>
# >> a.time_ago_in_words(Date.new)
# => "Worst of times"
# >> a.time_ago_in_words(Date.new)
# => "Time is but the stream I go a-fishing in"
# >> a.time_ago_in_words(Date.new)
# => "Lost time is never found again"
# >> a.time_ago_in_words(Date.new)
# => "How did it get so late so soon?"
# >> a.time_ago_in_words(Date.new)
# => "Best of times"
# >> 
```

Just drop that into any Rails project and enjoy.
