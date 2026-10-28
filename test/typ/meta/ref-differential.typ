#show ref: it => [#str(it.target)]
= First <my-anchor::>
= Second <my-anchor>
= Colon <tab:attributes>
= Dot <fig:sub.1>
= Unicode <α:β.γ>
= Multi <a:b.c:d>
= Short <fig>
= Longer <fig:alpha>
= Dotted <a.b.>
= Run One <a..b>
= Run Two <a.:b>
= Run Three <a::b>

Please see #ref(<my-anchor::>).
Please see #ref(<my-anchor>).
Please see @my-anchor::.
Please see @my-anchor:.
Please see @my-anchor.
Please see @tab:attributes, @fig:sub.1.
Please see @α:β.γ.
Please see @a:b.c:d.
Please see @fig.
Please see @fig:alpha.
Please see @fig:alpha and @fig:alpha.
Please see @my-anchor:: trailing
Please see @my-anchor: trailing
Please see @my-anchor. trailing
Please see @my-anchor:: trailing
Please see @my-anchor:[note]
Please see @my-anchor. [note]
Please see @my-anchor: [note]
Please see @my-anchor [note]
Please see @.
Please see @:
Please see @unknown:x.
Please see @unknown:x and @unknown:y.
Please see @a.b..
Please see @.x and @:x.
Please see @a..b, @a.:b, @a::b.
Please see @a.. and @a:b:.
