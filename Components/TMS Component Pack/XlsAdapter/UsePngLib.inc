//Who would have imagined png support could be so difficult?

{$IFDEF USEPNGLIB}
  {$IFDEF FLX_FPC}
     zlib, Graphics,
  {$ELSE}
    {$IFDEF DELPHI2008UP}
       pngimage,
       zlib,   //This changed in Delphi 2008 again. Now this units are included in delphi
    {$ELSE}
       flxpngimage,
       flxzlibpas,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
