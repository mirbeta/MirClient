{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Utils.UnicodeRangeInfo;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes;

type
  TdxUnicodeSubrange = record
    LowValue: Char;
    HiValue: Char;
    Bit: Integer;
    function ContainsChar(ACharacter: Char): Boolean;
  end;
  PdxUnicodeSubrange = ^TdxUnicodeSubrange;

  TdxUnicodeSubrangeArray = array of TdxUnicodeSubrange;

  TdxUnicodeRangeInfo = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FRanges: TdxUnicodeSubrangeArray;
    procedure SetCapacity(AValue: Integer);
  protected
    procedure AddSubrange(AStartCharacter, AEndCharacter: Char; ABit: Integer);
    procedure Grow;
    procedure PopulateSubranges;
  public
    constructor Create;
    destructor Destroy; override;
    function LookupSubrange(ACharacter: Char): PdxUnicodeSubrange;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
  end;

function UnicodeRangeInfo: TdxUnicodeRangeInfo;

implementation

uses
  Math, SysUtils;

var
  FUnicodeRangeInfo: TdxUnicodeRangeInfo;

function UnicodeRangeInfo: TdxUnicodeRangeInfo;
begin
  if FUnicodeRangeInfo = nil then
    FUnicodeRangeInfo := TdxUnicodeRangeInfo.Create;
  Result := FUnicodeRangeInfo;
end;

function TdxUnicodeSubrange.ContainsChar(ACharacter: Char): Boolean;
begin
  Result := ((ACharacter >= LowValue) and (ACharacter <= HiValue));
end;

{ UnicodeRangeInfo }

constructor TdxUnicodeRangeInfo.Create;
begin
  inherited Create;
  PopulateSubranges;
end;

destructor TdxUnicodeRangeInfo.Destroy;
begin
  FRanges := nil;
  inherited Destroy;
end;

procedure TdxUnicodeRangeInfo.PopulateSubranges;
begin
  Capacity := 135;
  AddSubrange(#$0000, #$007F, 0);
  AddSubrange(#$0080, #$00FF, 1);
  AddSubrange(#$0100, #$017F, 2);
  AddSubrange(#$0180, #$024F, 3);
  AddSubrange(#$0250, #$02AF, 4);
  AddSubrange(#$02B0, #$02FF, 5);
  AddSubrange(#$0300, #$036F, 6);
  AddSubrange(#$0370, #$03FF, 7);
  AddSubrange(#$0400, #$04FF, 9);
  AddSubrange(#$0500, #$052F, 9);
  AddSubrange(#$0530, #$058F, 10);
  AddSubrange(#$0590, #$05FF, 11);
  AddSubrange(#$0600, #$06FF, 13);
  AddSubrange(#$0700, #$074F, 71);
  AddSubrange(#$0750, #$077F, 13);
  AddSubrange(#$0780, #$07BF, 72);
  AddSubrange(#$07C0, #$07FF, 14);
  AddSubrange(#$0900, #$097F, 15);
  AddSubrange(#$0980, #$09FF, 16);
  AddSubrange(#$0A00, #$0A7F, 17);
  AddSubrange(#$0A80, #$0AFF, 18);
  AddSubrange(#$0B00, #$0B7F, 19);
  AddSubrange(#$0B80, #$0BFF, 20);
  AddSubrange(#$0C00, #$0C7F, 21);
  AddSubrange(#$0C80, #$0CFF, 22);
  AddSubrange(#$0D00, #$0D7F, 23);
  AddSubrange(#$0D80, #$0DFF, 73);
  AddSubrange(#$0E00, #$0E7F, 24);
  AddSubrange(#$0E80, #$0EFF, 25);
  AddSubrange(#$0F00, #$0FFF, 70);
  AddSubrange(#$1000, #$109F, 74);
  AddSubrange(#$10A0, #$10FF, 26);
  AddSubrange(#$1100, #$11FF, 28);
  AddSubrange(#$1200, #$137F, 75);
  AddSubrange(#$1380, #$139F, 75);
  AddSubrange(#$13A0, #$13FF, 76);
  AddSubrange(#$1400, #$167F, 77);
  AddSubrange(#$1680, #$169F, 78);
  AddSubrange(#$16A0, #$16FF, 79);
  AddSubrange(#$1700, #$171F, 84);
  AddSubrange(#$1720, #$173F, 84);
  AddSubrange(#$1740, #$175F, 84);
  AddSubrange(#$1760, #$177F, 84);
  AddSubrange(#$1780, #$17FF, 80);
  AddSubrange(#$1800, #$18AF, 81);
  AddSubrange(#$1900, #$194F, 93);
  AddSubrange(#$1950, #$197F, 94);
  AddSubrange(#$1980, #$19DF, 95);
  AddSubrange(#$19E0, #$19FF, 80);
  AddSubrange(#$1A00, #$1A1F, 96);
  AddSubrange(#$1B00, #$1B7F, 27);
  AddSubrange(#$1B80, #$1BBF, 112);
  AddSubrange(#$1C00, #$1C4F, 113);
  AddSubrange(#$1C50, #$1C7F, 114);
  AddSubrange(#$1D00, #$1D7F, 4);
  AddSubrange(#$1D80, #$1DBF, 4);
  AddSubrange(#$1DC0, #$1DFF, 6);
  AddSubrange(#$1E00, #$1EFF, 29);
  AddSubrange(#$1F00, #$1FFF, 30);
  AddSubrange(#$2000, #$206F, 31);
  AddSubrange(#$2070, #$209F, 32);
  AddSubrange(#$20A0, #$20CF, 33);
  AddSubrange(#$20D0, #$20FF, 34);
  AddSubrange(#$2100, #$214F, 35);
  AddSubrange(#$2150, #$218F, 36);
  AddSubrange(#$2190, #$21FF, 37);
  AddSubrange(#$2200, #$22FF, 38);
  AddSubrange(#$2300, #$23FF, 39);
  AddSubrange(#$2400, #$243F, 40);
  AddSubrange(#$2440, #$245F, 41);
  AddSubrange(#$2460, #$24FF, 42);
  AddSubrange(#$2500, #$257F, 43);
  AddSubrange(#$2580, #$259F, 44);
  AddSubrange(#$25A0, #$25FF, 45);
  AddSubrange(#$2600, #$26FF, 46);
  AddSubrange(#$2700, #$27BF, 47);
  AddSubrange(#$27C0, #$27EF, 38);
  AddSubrange(#$27F0, #$27FF, 37);
  AddSubrange(#$2800, #$28FF, 82);
  AddSubrange(#$2900, #$297F, 37);
  AddSubrange(#$2980, #$29FF, 38);
  AddSubrange(#$2A00, #$2AFF, 38);
  AddSubrange(#$2B00, #$2BFF, 37);
  AddSubrange(#$2C00, #$2C5F, 97);
  AddSubrange(#$2C60, #$2C7F, 29);
  AddSubrange(#$2C80, #$2CFF, 8);
  AddSubrange(#$2D00, #$2D2F, 26);
  AddSubrange(#$2D30, #$2D7F, 98);
  AddSubrange(#$2D80, #$2DDF, 75);
  AddSubrange(#$2DE0, #$2DFF, 9);
  AddSubrange(#$2E00, #$2E7F, 31);
  AddSubrange(#$2E80, #$2EFF, 59);
  AddSubrange(#$2F00, #$2FDF, 59);
  AddSubrange(#$2FF0, #$2FFF, 59);
  AddSubrange(#$3000, #$303F, 48);
  AddSubrange(#$3040, #$309F, 49);
  AddSubrange(#$30A0, #$30FF, 50);
  AddSubrange(#$3100, #$312F, 51);
  AddSubrange(#$3130, #$318F, 52);
  AddSubrange(#$3190, #$319F, 59);
  AddSubrange(#$31A0, #$31BF, 51);
  AddSubrange(#$31C0, #$31EF, 61);
  AddSubrange(#$31F0, #$31FF, 50);
  AddSubrange(#$3200, #$32FF, 54);
  AddSubrange(#$3300, #$33FF, 55);
  AddSubrange(#$3400, #$4DBF, 59);
  AddSubrange(#$4DC0, #$4DFF, 99);
  AddSubrange(#$4E00, #$9FFF, 59);
  AddSubrange(#$A000, #$A48F, 83);
  AddSubrange(#$A490, #$A4CF, 83);
  AddSubrange(#$A500, #$A63F, 12);
  AddSubrange(#$A640, #$A69F, 9);
  AddSubrange(#$A700, #$A71F, 5);
  AddSubrange(#$A720, #$A7FF, 29);
  AddSubrange(#$A800, #$A82F, 100);
  AddSubrange(#$A840, #$A87F, 53);
  AddSubrange(#$A880, #$A8DF, 115);
  AddSubrange(#$A900, #$A92F, 116);
  AddSubrange(#$A930, #$A95F, 117);
  AddSubrange(#$AA00, #$AA5F, 118);
  AddSubrange(#$AC00, #$D7AF, 56);
  AddSubrange(#$D800, #$DFFF, 57);
  AddSubrange(#$E000, #$F8FF, 60);
  AddSubrange(#$F900, #$FAFF, 61);
  AddSubrange(#$FB00, #$FB4F, 62);
  AddSubrange(#$FB50, #$FDFF, 63);
  AddSubrange(#$FE00, #$FE0F, 91);
  AddSubrange(#$FE10, #$FE1F, 65);
  AddSubrange(#$FE20, #$FE2F, 64);
  AddSubrange(#$FE30, #$FE4F, 65);
  AddSubrange(#$FE50, #$FE6F, 66);
  AddSubrange(#$FE70, #$FEFF, 67);
  AddSubrange(#$FF00, #$FFEF, 68);
  AddSubrange(#$FFF0, #$FFFF, 69);

end;

procedure TdxUnicodeRangeInfo.AddSubrange(AStartCharacter, AEndCharacter: Char; ABit: Integer);
begin
  if FCount = FCapacity then
    Grow;
  with FRanges[FCount] do
  begin
    LowValue := AStartCharacter;
    HiValue  := AEndCharacter;
    Bit      := ABit;
  end;
  Inc(FCount);
end;

procedure TdxUnicodeRangeInfo.Grow;
begin
  Capacity := Capacity + Max(Capacity div 2, 64);
end;

function TdxUnicodeRangeInfo.LookupSubrange(ACharacter: Char): PdxUnicodeSubrange;
var
  I, H, L: Integer;
begin
  Result := nil;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if ACharacter < FRanges[I].LowValue then
      H := I - 1
    else
      if ACharacter > FRanges[I].HiValue then
        L := I + 1
      else
      begin
        Result := @FRanges[I];
        Break;
      end;
  end;
end;

procedure TdxUnicodeRangeInfo.SetCapacity(AValue: Integer);
begin
  if FCapacity <> AValue then
  begin
    FCapacity := AValue;
    SetLength(FRanges, Capacity);
  end;
end;

initialization

finalization
  FreeAndNil(FUnicodeRangeInfo);

end.
