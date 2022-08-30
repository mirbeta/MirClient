{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCoreLibrary                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORELIBRARY AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxCharacters;

{$I cxVer.inc}

interface

uses
  Character;

type

  { TdxCharacters }

  TdxCharacters = record
  public const
    Nothing                    = #0;
    Dot                        = '.';
    Colon                      = ':';
    Underscore                 = '_';
    EqualSign                  = '=';
    MiddleDot                  = #$00B7;
    Dash                       = '-';
    ParagraphMark              = #$000D;
    SectionMark                = #$001D;
    Hyphen                     = #$001F;
    TabMark                    = #$0009;
    NonBreakingSpace           = #$00A0;
    Space                      = ' ';
    EmSpace                    = #$2003;
    EnSpace                    = #$2002;
    QmSpace                    = #$2005;
    LineBreak                  = #$000B;
    PageBreak                  = #$000C;
    ColumnBreak                = #$000E;
    ObjectMark                 = #$FFFC;
    FloatingObjectMark         = #$0008;
    EmDash                     = #$2014;
    EnDash                     = #$2013;
    Bullet                     = #$2022;
    LeftSingleQuote            = #$2018;
    RightSingleQuote           = #$2019;
    LeftDoubleQuote            = #$201C;
    RightDoubleQuote           = #$201D;
    PilcrowSign                = #$00B6;
    CurrencySign               = #$00A4;
    CopyrightSymbol            = #$00A9;
    TrademarkSymbol            = #$2122;
    OptionalHyphen             = #$00AD;
    RegisteredTrademarkSymbol  = #$00AE;
    Ellipsis                   = #$2026;
    OpeningSingleQuotationMark = #$2018;
    ClosingSingleQuotationMark = #$2019;
    OpeningDoubleQuotationMark = #$201C;
    ClosingDoubleQuotationMark = #$201D;
    SeparatorMark              = '|';
    PasswordBullet             = #$25CF;
  public
    class function IsCharDash(C: Char): Boolean; inline; static;
    class function IsCharSpace(C: Char): Boolean; inline; static;
    class function IsLetter(C: Char): Boolean; inline; static;
    class function IsLetterOrDigit(C: Char): Boolean; inline; static;
    class function IsLower(C: Char): Boolean; inline; static;
    class function IsNumber(C: Char): Boolean; inline; static;
    class function IsUpper(C: Char): Boolean; inline; static;
    class function IsWhiteSpace(C: Char): Boolean; inline; static;
    class function ToLower(C: Char): Char; inline; static;
    class function ToUpper(C: Char): Char; inline; static;
  end;

implementation

class function TdxCharacters.IsCharDash(C: Char): Boolean;
begin
  Result := (C = Dash) or (C = EmDash) or (C = EnDash);
end;

class function TdxCharacters.IsCharSpace(C: Char): Boolean;
begin
  Result := (C = Space) or (C = EmSpace) or (C = EnSpace);
end;

class function TdxCharacters.IsLetter(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsLetter;
{$ELSE}
  Result := TCharacter.IsLetter(C);
{$ENDIF}
end;

class function TdxCharacters.IsLetterOrDigit(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsLetterOrDigit;
{$ELSE}
  Result := TCharacter.IsLetterOrDigit(C);
{$ENDIF}
end;

class function TdxCharacters.IsLower(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsLower;
{$ELSE}
  Result := TCharacter.IsLower(C);
{$ENDIF}
end;

class function TdxCharacters.IsNumber(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsNumber;
{$ELSE}
  Result := TCharacter.IsNumber(C);
{$ENDIF}
end;

class function TdxCharacters.IsUpper(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsUpper;
{$ELSE}
  Result := TCharacter.IsUpper(C);
{$ENDIF}
end;

class function TdxCharacters.IsWhiteSpace(C: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := C.IsWhiteSpace;
{$ELSE}
  Result := TCharacter.IsWhiteSpace(C);
{$ENDIF}
end;

class function TdxCharacters.ToLower(C: Char): Char;
begin
{$IFDEF DELPHIXE4}
  Result := C.ToLower;
{$ELSE}
  Result := TCharacter.ToLower(C);
{$ENDIF}
end;

class function TdxCharacters.ToUpper(C: Char): Char;
begin
{$IFDEF DELPHIXE4}
  Result := C.ToUpper;
{$ELSE}
  Result := TCharacter.ToUpper(C);
{$ENDIF}
end;

end.
