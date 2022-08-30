object Form1: TForm1
  Left = 236
  Top = 214
  Width = 983
  Height = 596
  Caption = 'RTC Scripting Engine Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 474
    Top = 0
    Width = 4
    Height = 551
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 551
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    DesignSize = (
      474
      551)
    object Label1: TLabel
      Left = 10
      Top = 12
      Width = 371
      Height = 16
      Caption = 
        'Script code to compile and execute ... (script always in <? ... ' +
        '?> )'
    end
    object Memo1: TMemo
      Left = 7
      Top = 36
      Width = 459
      Height = 505
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        '1. Claudia Schiffer example:'
        '<h1>'
        '<?'
        '$Name := '#39'Schiffer'#39';'
        '// do something here'
        'if $Name = '#39'Schiffer'#39' then'
        '    '#39'Claudia'#39
        ' else'
        '    $Name;'
        '?>'
        '</h1>'
        '------------'
        '1b. Claudia Schiffer example using a code block:'
        '<h1>'
        '<?'
        '$Vorname := code '
        '  if $Name = '#39'Schiffer'#39' then'
        '    '#39'Claudia'#39
        '  else'
        '    '#39'unknown'#39';'
        ''
        '$Name := '#39'Schiffer'#39';'
        '$Vorname " " $Name ?>'
        '<? $Name := '#39'Becker'#39';'
        ''
        '// See how the $Vorname variable changes ...'
        '$Vorname " " $Name ?>'
        '</h1>'
        '--------------------------'
        '2. Claudia Shiffer sample:'
        '<?'
        'function $GetVorname'
        'begin'
        ' if $Name = '#39'Schiffer'#39' then'
        '    $result := '#39'Claudia'#39
        ' else'
        '    $result := '#39'unknown'#39';'
        'end;'
        '?>'
        '<h1><? $GetVorname( Name:'#39'Schiffer'#39') ?></h1>'
        ''
        '--------------------------'
        '3. Claudia Shiffer sample:'
        '<?'
        'function $GetVorname $result :='
        '  if $Name = '#39'Schiffer'#39' then'
        '    '#39'Claudia'#39
        '  else'
        '    '#39'unknown'#39';'
        '?>'
        '<h1><? $GetVorname( Name:'#39'Schiffer'#39') ?></h1>'
        ''
        '---------------------------'
        '4. Claudia Schiffer sample:'
        '<?'
        'function $GetVorname $result :='
        '  if $Params.0 = '#39'Schiffer'#39' then'
        '    '#39'Claudia'#39
        '  else'
        '    '#39'unknown'#39';'
        '?>'
        '<h1><? // this last function is NOT expecting names'
        '  $GetVorname('#39'Schiffer'#39') ?></h1>'
        ''
        '--------------------------------'
        '5. Min and Max example without parameter names:'
        '<?'
        'function $max $result := '
        '  if $params.0 > $params.1 then $params.0'
        '  else $params.1;'
        ''
        'function $min $result := '
        '  if $params.0 < $params.1 then $params.0'
        '  else $params.1;'
        ''
        '$max(10,20);" - ";'
        '$max(1, $max(3,5));'#39' - '#39';'
        '$min(10,20);" ! "'
        '?>'
        ''
        '----------------------------------'
        '6. Several different Min/Max examples'
        '<?'
        'function $max $result :='
        'begin'
        '  if $PARAMS <> null then'
        '    { if $params.0 > $params.1 then $params.0 '
        '      else $params.1 }'
        '  else'
        '    { if $A > $B then $A '
        '      else $B }'
        'end;'
        ''
        'function $min $result :='
        'begin'
        '  if $PARAMS <> null then'
        '    { if $params.0 < $params.1 then $params.0 '
        '      else $params.1 }'
        '  else'
        '    { if $A < $B then $A '
        '      else $B }'
        'end;'
        ''
        '?>Using "max"  with parameter names: <? $max(a:10, b:20) ?>'
        
          'Using "max" with index and "min" with names: <? $max(1, $min(b:3' +
          ', a:5))?>'
        'Using "min with index: <? $min(10,20) ?>'
        ''
        '------------------'
        '7. Accessing fields in records'
        '<?'
        '// Defining a new local function "$fn"'
        'function $fn $result := (x:10, y:20);'
        '?>'
        'Calling local function "$fn" '
        'and working with the result (record).'
        '<?'
        '$x := $fn;'
        '$x.count,'#39' - '#39', $x.x, '#39' - '#39', $x.y; '#39' / '#39';'
        ''
        '// You can even override a function'
        'function $fn $result := (30, 40);'
        '?>'
        'From this point on, we will be calling the new function'
        '<?'
        '$x := $fn;'
        '$x.count,'#39' - '#39', $x.0, '#39' - '#39', $x.1; '#39' / '#39
        '?>'
        'You can also assign arrays directly to variables'
        '<? $x := (50, 60,70,80,90,100);'
        ' (* Now that we got a few more elements,'
        '    Let'#39's write all elements out using a "for" loop ... *)'
        ''
        'for $i := 0 to $x.count-1 do'
        '// since this is a single command, we don'#39't need "begin/end"'
        '  $x($i) + if $i<$x.count-1 then '#39', '#39';'
        '?>'
        ''
        '----------------------------'
        '8. Using $Result to caculate the output'
        '<? function $test  if $params <> nil then'
        'begin'
        '  $result := 0;'
        '  for $i := 1 to $params.0 do $result += $i;'
        'end;?>'
        'Sum of the first 1000 integer numbers is <?$test(1000)?>'
        ''
        '---------------------------'
        '9. Sending parameters as an array '
        '   and enumerating through the array'
        '<? function $sum if $params <>nil then'
        '  for $i := 0 to $params.count-1 do'
        '    $result += $params($i);'
        ''
        '$sum(8,12,36,69,12);" = ";8+12+36+69+12'
        '?>'
        ''
        '--------------------------------------'
        '10. Another array enumeration example:'
        '<? function $test'
        '  begin'
        '  if $params <> nil then'
        '    for $k := 0 to $params.count-1 do'
        '      for $i := 1 to $params($k) do $result += $i;'
        '  end;'
        ''
        '$test(300,100,10,1) "=" '
        '$test(300)+$test(100)+$test(10)+$test(1) ?>'
        ''
        '-------------------------'
        '11. Simple calculations:'
        '<? -12+10*10; " = "; (-12)+(10*10); " = "; 10*10-12 ?>'
        '<? $x := (a:(10,20),b:(30,40));'
        '$x.a.0*$x.a.1+$x.b.0*$x.b.1 " = " 10*20+30*40 " = "'
        '$('#39'x'#39').a(0) * $x('#39'a'#39').1 + $x('#39'b.0'#39') * $x.b(1) ?>'
        ''
        '-------------------------------------'
        '12. Using FOR and "#" to generate a string of special chars:'
        '<? for $i:= 32 to 64 do #$i ?>'
        ''
        '-------------------'
        '13. Testing recursive calls'
        '<? function $test $result := if $params.0 >0 then'
        #9'$params.0+$test($params.0-1);'
        ''
        '(* Do not overdo it, or you will generate a Stack Overflow,'
        '   which will make the application crash (no warning).'
        ''
        '   Since the function call Stack is quite limited,'
        '   you should avoid recursive calls wherever possible, '
        '   especially since 99% implementations using recursive '
        '   calls can be implemented using loops and variables. *)'
        ''
        '// Here is the same function without recursive calls ...'
        'function $test2'
        '  begin'
        '  $result:= 0.0;'
        '  for $i := 0 to $params.0 do'
        '    $result += $i;'
        '  end;'
        ''
        '// Both functions should return the same result'
        '$test(123) " = " $test2(123) ?>'
        ''
        '------------------'
        '14. Testing calls to RTC Functions (written in Delphi)'
        '<?'
        'function $RandomList'
        '  begin'
        '  $Result := '#39#39'; '
        '  for $i := 1 to $cnt do'
        '    begin'
        '    $x := random($max);'
        '    if $Result<>'#39#39' then'
        '      $Result += '#39','#39'+$x'
        '    else'
        '      $Result := $x;'
        '    end;'
        '  end;'
        '?>Here are our 10 random numbers (0-99):'
        '<? $RandomList(cnt:10, max:100) ?>'
        'RTC Function Time rerturns <? time ?>, '
        'RTC Function Date says <? date ?> and'
        'RTC Function Now shows <? Now ?>.'
        ''
        'And here is a single random number:'
        '<? random(100)+random(100) ?>'
        ''
        '15. Test using DataSets in script'
        '------------------------'
        '<? '
        '$x := new dataset;'
        ''
        'for $i := 1 to 30 do'
        '  begin'
        '  $x.append;'
        '  for $j := 1 to 15 do'
        '    $x('#39'f'#39'+$j) := $i*$j;'
        '  end;'
        '?>'
        '"While" loop using First, Next, EOF:'
        '<?'
        '$x.first;'
        'while not $x.eof do'
        '  begin'
        '  '#39'Row '#39' $x.row '#39') '#39
        '  for $i:=0 to $x.fieldcount-1 do'
        '    begin'
        '    $x($i);'
        '    if $i<$x.fieldcount-1 then '#39', '#39';'
        '    end;?>'
        '<?$x.next;'
        '  end;?>'
        
          '"For" loop using RowCount and Row and a codeblock for indirect a' +
          'ccess:'
        '<?'
        '// now $rec points to $x'
        '$rec := code $x;'
        ''
        'for $i := 0 to $rec.rowcount-1 do'
        '  begin'
        '  $rec.row := $i;'
        '  '#39'Row '#39' $rec.row '#39') '#39
        '  for $i:=0 to $rec.fieldcount-1 do'
        '    begin'
        '    $rec($i);'
        '    if $i<$rec.fieldcount-1 then '#39', '#39';'
        '    end;?>'
        '<?end;?>'
        'Here is the same DataSet using RTC output:'
        '<? $x ?>'
        'And the same when using the $rec "pointer":'
        '<? $rec ?>'
        ''
        '16. Test writing and calling a script function'
        '-----------------'
        '<? function $PolyArea'
        'begin'
        ' (* Return the area of a polygon with N vertices, where'
        '    the input consists of coordinates passed as records (X,Y) *)'
        ''
        '  if $params<>null then'
        '    begin'
        '    $Result := 0.0;'
        '    For $I := 0 to $params.count-1 do'
        '      begin'
        '      $cord1 := $params($I);'
        '      if $I<$params.count-1 then'
        '        $cord2 := $params($I+1)'
        '      else'
        '        $cord2 := $params(0);'
        '      $Result += $cord1.x * $cord2.y - $cord2.x * $cord1.y;'
        '      end;'
        '   if $Result < 0 then $Result := -$Result;'
        ''
        '   $Result /= 2.0;'
        '   end;'
        'end;'
        ''
        '$PolyArea( (x:0,y:0), (x:10,y:0), (x:10, y:10) ); ?>'
        ''
        
          '17. The same script function working with polymorphic parameters' +
          ':'
        '---------------------'
        '<? '
        'function $PolyArea'
        'begin'
        ' (* Return the area of a polygon with N vertices, where'
        '    the input consists of coordinates passed as records (X,Y) *)'
        ''
        '  function $GetN $result :='
        '    if $params.0($params.1) <> NULL then $params.0($params.1)'
        '    else $params.0($params.2);'
        ''
        '  if $params<>null then'
        '    begin'
        '    $Result := 0.0;'
        '    For $I := 0 to $params.count-1 do'
        '      begin'
        ''
        '      $cord1 := $params($I);'
        '      if $I<$params.count-1 then'
        '        $cord2 := $params($I+1)'
        '      else'
        '        $cord2 := $params(0);'
        '      $Result += $GetN($cord1,0,'#39'x'#39') * $GetN($cord2,1,'#39'y'#39')'
        '               - $GetN($cord2,0,'#39'x'#39') * $GetN($cord1,1,'#39'y'#39');'
        '      end;'
        '   if $Result < 0 then $Result := -$Result;'
        ''
        '   $Result /= 2.0;'
        '   end;'
        'end; '
        ''
        'PolyArea((0,0), (10,0), (10,10)), '#39' = '#39','
        'PolyArea((x:0,y:0), (x:10,y:0), (x:10,y:10)), '#39' = '#39','
        'PolyArea((0,0), (x:10,y:0), (10,10)) ?>'
        ''
        '18. The same script function using codeblocks:'
        '---------------------'
        '<? '
        'function $PolyArea'
        'begin'
        ' (* Return the area of a polygon with N vertices, where'
        '    the input consists of coordinates passed as records (X,Y) *)'
        ''
        
          ' (* We can declare our helper function $GetN inside the function' +
          ', so it '
        
          '    is invisible from outside andexists only inside this functio' +
          'n. *)'
        ''
        '  function $GetN $result :='
        '    if $params.0($params.1) <> NULL then'
        '       $params.0($params.1)'
        '    else '
        '       $params.0($params.2);'
        ''
        '  if $params<>null then'
        '    begin'
        '    $Result := 0.0;'
        ''
        '    (* we can declare our code block $next here,'
        
          '       even before all variables used in code block were defined' +
          '.'
        '       We will use this codeblock as "next index pointer". *)'
        ''
        '    $next := code'
        '      if $I<$params.count-1 then $I+1 else 0;'
        ''
        '    For $I := 0 to $params.count-1 do'
        '      begin'
        ''
        
          '      (* using code blocks also avoids making copies of the orig' +
          'inal structure and allows'
        
          '         midifying the original structure through the "pointer" ' +
          'variable, if necessary. *)'
        '     '
        '      $cord1 := code $params($I);'
        '      $cord2 := code $params($next);'
        ''
        '      $Result += $GetN($cord1,0,'#39'x'#39') * $GetN($cord2,1,'#39'y'#39')'
        '               - $GetN($cord2,0,'#39'x'#39') * $GetN($cord1,1,'#39'y'#39');'
        '      end;'
        ''
        '   if $Result < 0 then $Result := -$Result;'
        ''
        '   $Result /= 2.0;'
        '   end;'
        'end; '
        ''
        '?>This triangle is 50% of a 10 x 10 square:'
        '<?PolyArea((0,0), (10,0), (10,10)), '#39' = '#39','
        'PolyArea((x:0,y:0), (x:10,y:0), (x:10,y:10)), '#39' = '#39','
        'PolyArea((0,0), (x:10,y:0), (10,10)) ?>')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WantTabs = True
      WordWrap = False
    end
  end
  object Panel2: TPanel
    Left = 478
    Top = 0
    Width = 487
    Height = 551
    Align = alClient
    TabOrder = 1
    DesignSize = (
      487
      551)
    object lblTime: TLabel
      Left = 153
      Top = 12
      Width = 46
      Height = 16
      Caption = 'Time: ...'
    end
    object Button1: TButton
      Left = 12
      Top = 6
      Width = 132
      Height = 31
      Caption = 'Execute Script (left)'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Memo2: TMemo
      Left = 8
      Top = 40
      Width = 473
      Height = 505
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object RtcScriptEngine1: TRtcScriptEngine
    FunctionGroup = FunctionGroup
    ScriptOpen = '<?'
    ScriptClose = '?>'
    MaxCodeDepth = 500
    MaxRecursion = 200
    MaxLoopCount = 100000
    Left = 432
    Top = 47
  end
  object FunctionGroup: TRtcFunctionGroup
    Left = 436
    Top = 98
  end
  object FnTime: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'Time'
    OnExecute = FnTimeExecute
    Left = 440
    Top = 148
  end
  object FnDate: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'Date'
    OnExecute = FnDateExecute
    Left = 492
    Top = 148
  end
  object FnNow: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'Now'
    OnExecute = FnNowExecute
    Left = 540
    Top = 148
  end
  object FnRandom: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'Random'
    OnExecute = FnRandomExecute
    Left = 520
    Top = 190
  end
  object FnPolyArea: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'PolyArea'
    OnExecute = FnPolyAreaExecute
    Left = 452
    Top = 190
  end
  object DummyConnection: TRtcHttpServer
    Left = 480
    Top = 48
  end
end
