{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}
unit cxConverterUtils;

{$I cxVer.inc}

interface

uses
  Classes, StdCtrls, {$IFDEF DELPHI16} UITypes,{$ENDIF} Graphics;

  function CompareFonts(AFont1, AFont2: TFont): Boolean;
  function ConvertAlignment(const AValue: string): TAlignment;
  function ConvertCharCase(const AValue: string): TEditCharCase;
  function ConvertScrollStyle(const AValue: string): TScrollStyle;
  function DefaultFont(AFont: TFont): Boolean;

implementation

uses
  dxCore;

function CompareFonts(AFont1,
  AFont2: TFont): Boolean;
begin
  if (AFont1 = nil) or (AFont2 = nil) then
  begin
    Result := True;
    Exit;
  end;
  with AFont1 do
  begin
    Result :=
      (Charset = AFont2.Charset) and
      (Color = AFont2.Color) and
      (Height = AFont2.Height) and
      (Name = AFont2.Name) and
      (Pitch = AFont2.Pitch) and
      (Size = AFont2.Size) and
      (Style = AFont2.Style);
  end;
end;

function ConvertAlignment(const AValue: string): TAlignment;
begin
  if AValue = 'taLeftJustify' then
    Result := taLeftJustify
  else if AValue = 'taRightJustify' then
    Result := taRightJustify
  else if AValue = 'taCenter' then
    Result := taCenter
  else
    Result := taLeftJustify;
end;

function ConvertCharCase(const AValue: string): TEditCharCase;
begin
  if AValue = 'ecNormal' then
    Result := ecNormal
  else if AValue = 'ecUpperCase' then
    Result := ecUpperCase
  else if AValue = 'ecLowerCase' then
    Result := ecLowerCase
  else
    Result := ecNormal;
end;

function ConvertScrollStyle(const AValue: string): TScrollStyle;
begin
  if AValue = 'ssNone' then
    Result := ssNone
  else if AValue = 'ssHorizontal' then
    Result := ssHorizontal
  else if AValue = 'ssVertical' then
    Result := ssVertical
  else if AValue = 'ssBoth' then
    Result := ssBoth
  else
    Result := ssNone;
end;

function DefaultFont(AFont: TFont): Boolean;
begin
  if AFont = nil then
  begin
    Result := True;
    Exit;
  end;
  with AFont do
    Result :=
      (Pitch = DefFontData.Pitch) and
      (Style = DefFontData.Style) and
      (Charset = DefFontData.Charset) and
      (Name = dxShortStringToString(DefFontData.Name)) and
      (Color = clWindowText) and
      (Size = 8);
end;

end.
