    {$IFDEF VER100}
    compiler := 'Delphi 3';
    {$ENDIF}
    {$IFDEF VER110}
    compiler := 'C++Builder 3';
    {$ENDIF}
    {$IFDEF VER120}
    compiler := 'Delphi 4';
    {$ENDIF}
    {$IFDEF VER125}
    compiler := 'C++Builder 4';
    {$ENDIF}
    {$IFDEF VER130}
    {$IFDEF BCB}
    compiler := 'C++Builder 5';
    {$ELSE}
    compiler := 'Delphi 5';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER140}
    {$IFDEF BCB}
    compiler := 'C++Builder 6';
    {$ELSE}
    compiler := 'Delphi 6';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER150}
    {$IFDEF BCB}
    compiler := '';
    {$ELSE}
    compiler := 'Delphi 7';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER160}
    compiler := 'Delphi 8';
    {$ENDIF}
    {$IFDEF VER170}
    compiler := 'Delphi 2005';
    {$ENDIF}
    {$IFDEF VER180}
    {$IFDEF BCB}
    compiler := 'C++Builder 2006';
    {$ELSE}
    compiler := 'Delphi 2006';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER185}
    {$IFDEF BCB}
    compiler := 'C++Builder 2007';
    {$ELSE}
    compiler := 'Delphi 2007';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER200}
    {$IFDEF BCB}
    compiler := 'C++Builder 2009';
    {$ELSE}
    compiler := 'Delphi 2009';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER210}
    {$IFDEF BCB}
    compiler := 'C++Builder 2010';
    {$ELSE}
    compiler := 'Delphi 2010';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER220}
    {$IFDEF BCB}
    compiler := 'C++Builder XE';
    {$ELSE}
    compiler := 'Delphi XE';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER230}
    {$IFDEF BCB}
    compiler := 'C++Builder XE2';
    {$ELSE}
    compiler := 'Delphi XE2';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER240}
    {$IFDEF BCB}
    compiler := 'C++Builder XE3';
    {$ELSE}
    compiler := 'Delphi XE3';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER250}
    {$IFDEF BCB}
    compiler := 'C++Builder XE4';
    {$ELSE}
    compiler := 'Delphi XE4';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER260}
    {$IFDEF BCB}
    compiler := 'C++Builder XE5';
    {$ELSE}
    compiler := 'Delphi XE5';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER270}
    {$IFDEF BCB}
    compiler := 'C++Builder XE6';
    {$ELSE}
    compiler := 'Delphi XE6';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER280}
    {$IFDEF BCB}
    compiler := 'C++Builder XE7';
    {$ELSE}
    compiler := 'Delphi XE7';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER290}
    {$IFDEF BCB}
    compiler := 'C++Builder XE8';
    {$ELSE}
    compiler := 'Delphi XE8';
    {$ENDIF}
    {$ENDIF}