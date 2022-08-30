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

unit dxRichEdit.Utils.Token;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes,
  Generics.Defaults, Generics.Collections;

type
  TdxTokenKind = (
    Eof,
    OpEQ,
    OpNEQ,
    OpLOW,
    OpLOWEQ,
    OpHI,
    OpHIEQ,
    OpPLUS,
    OpMINUS,
    OpMUL,
    OpDIV,
    OpPOW,
    OpenParenthesis,
    CloseParenthesis,
    Simple,
    DocPropertyInfoCommon,
    DocPropertyCategory,
    DocProperty,
    DocumentInformation,
    Eq,
    DateAndTimeFormattingSwitchBegin,
    GeneralFormattingSwitchBegin,
    NumericFormattingSwitchBegin,
    CommonStringFormatSwitchBegin,
    Text,
    QuotedText,
    FieldSwitchCharacter,
    Constant,
    Percent,
    SeparatorChar,
    FunctionName,
    Template,
    Invalid = $ff);

  { IdxToken }

  IdxToken = interface
  ['{E35B0BC5-3877-4AD6-9A11-B14D9901DD33}']
    function GetActualKind: TdxTokenKind;
    function GetKind: Integer;
    function GetTokenValue: string;
    function GetPosition: Integer;
    function GetLength: Integer;
    function GetNext: IdxToken;

    procedure SetTokenValue(const Value: string);
    procedure SetLength(const Value: Integer);
    procedure SetNext(const Value: IdxToken);

    property ActualKind: TdxTokenKind read GetActualKind;
    property Kind: Integer read GetKind;
    property Value: string read GetTokenValue write SetTokenValue;
    property Position: Integer read GetPosition;
    property Length: Integer read GetLength write SetLength;
    property Next: IdxToken read GetNext write SetNext;
  end;

  { TdxTokenList }

  TdxTokenList = class(TInterfaceList)
  private
    function GetItem(Index: Integer): IdxToken;
  public
    function First: IdxToken; reintroduce;
    function Last: IdxToken; reintroduce;

    property Items[Index: Integer]: IdxToken read GetItem; default;
  end;

  { TdxToken }

  TdxToken = class(TInterfacedObject, IdxToken)
  strict private
    FKind: Integer;
    FPosition: Integer;
    FLength: Integer;
    FTokenValue: string;
    FNext: IdxToken;
  private
    function GetActualKind: TdxTokenKind;
    function GetKind: Integer;
    function GetTokenValue: string;
    function GetPosition: Integer;
    function GetLength: Integer;
    function GetNext: IdxToken;
    procedure SetTokenValue(const Value: string);
    procedure SetLength(const Value: Integer);
    procedure SetNext(const Value: IdxToken);
  public
    constructor Create(AKind: TdxTokenKind;
      APosition: Integer = 0;
      const Value: string = ''; ALength: Integer = 1);

    property ActualKind: TdxTokenKind read GetActualKind;
    property Kind: Integer read GetKind;
    property Value: string read GetTokenValue write SetTokenValue;
    property Position: Integer read GetPosition;
    property Length: Integer read GetLength write SetLength;
    property Next: IdxToken read GetNext write SetNext;
  end;

  { IdxArgument }

  IdxArgument = interface
    function GetValue: string;
    function GetStartPosition: Integer;
    function GetLength: Integer;

    property Value: string read GetValue;
    property StartPosition: Integer read GetStartPosition;
    property Length: Integer read GetLength;
  end;

  { TdxArgument }

  TdxArgument = class(TInterfacedObject, IdxArgument)
  private
    FToken: IdxToken;
    function GetValue: string;
    function GetStartPosition: Integer;
    function GetLength: Integer;
  public
    constructor Create(const AToken: IdxToken);

    property Value: string read GetValue;
    property StartPosition: Integer read GetStartPosition;
    property Length: Integer read GetLength;
  end;

  TdxArgumentCollection = TList<IdxArgument>;

implementation

{ TdxTokenList }

function TdxTokenList.First: IdxToken;
begin
  Result := inherited First as IdxToken;
end;

function TdxTokenList.Last: IdxToken;
begin
  Result := inherited Last as IdxToken;
end;

function TdxTokenList.GetItem(Index: Integer): IdxToken;
begin
  Result := inherited Items[Index] as IdxToken;
end;

{ TdxToken }

constructor TdxToken.Create(AKind: TdxTokenKind;
  APosition: Integer = 0; const Value: string = ''; ALength: Integer = 1);
begin
  FKind := Ord(AKind);
  FPosition := APosition;
  FTokenValue := Value;
  FLength := ALength;
  FNext := nil;
end;

function TdxToken.GetActualKind: TdxTokenKind;
begin
  Result := TdxTokenKind(FKind);
end;

function TdxToken.GetKind: Integer;
begin
  Result := FKind;
end;

function TdxToken.GetTokenValue: string;
begin
  Result := FTokenValue;
end;

function TdxToken.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TdxToken.GetLength: Integer;
begin
  Result := FLength;
end;

function TdxToken.GetNext: IdxToken;
begin
  Result := FNext;
end;

procedure TdxToken.SetTokenValue(const Value: string);
begin
  FTokenValue := Value;
end;

procedure TdxToken.SetLength(const Value: Integer);
begin
  FLength := Value;
end;

procedure TdxToken.SetNext(const Value: IdxToken);
begin
  FNext := Value;
end;

{ TdxArgument }

constructor TdxArgument.Create(const AToken: IdxToken);
begin
  FToken := AToken;
end;

function TdxArgument.GetValue: string;
begin
  Result := FToken.Value;
end;

function TdxArgument.GetStartPosition: Integer;
begin
  Result := FToken.Position;
end;

function TdxArgument.GetLength: Integer;
begin
  Result := FToken.Length;
end;

end.
