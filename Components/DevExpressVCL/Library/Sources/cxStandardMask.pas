{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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
unit cxStandardMask;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils;

type
  { TcxStandardMaskCustomItem }

  TcxStandardMaskCustomItem = class
  public
    function Check(var AChar: Char): Boolean; virtual; abstract;
  end;

  { TcxStandardMaskLiteralItem }

  TcxStandardMaskLiteralItem = class(TcxStandardMaskCustomItem)
  private
    FLiteral: Char;
  public
    constructor Create(ALiteral: Char);
    function Check(var AChar: Char): Boolean; override;
    property Literal: Char read FLiteral;
  end;

  { TcxCaseControl }

  TcxCaseControl = (ccUpperCase, ccLowerCase, ccUserCase);

  { TcxStandardMaskManyItem }

  TcxStandardMaskManyItem = class(TcxStandardMaskCustomItem)
  private
    FCaseControl: TcxCaseControl;
    FOptional: Boolean;
  protected
    procedure DoCaseControl(var AChar: Char);
  public
    constructor Create(AOptional: Boolean; ACaseControl: TcxCaseControl);
    property Optional: Boolean read FOptional;
  end;

  { TcxStandardMaskAlphaItem }

  TcxStandardMaskAlphaItem = class(TcxStandardMaskManyItem)
  public
    function Check(var AChar: Char): Boolean; override;
  end;

  { TcxStandardMaskAlphaNumericItem }

  TcxStandardMaskAlphaNumericItem = class(TcxStandardMaskManyItem)
  public
    function Check(var AChar: Char): Boolean; override;
  end;

  { TcxStandardMaskASCIIItem }

  TcxStandardMaskASCIIItem = class(TcxStandardMaskManyItem)
  public
    function Check(var AChar: Char): Boolean; override;
  end;

  { TcxStandardMaskNumericItem }

  TcxStandardMaskNumericItem = class(TcxStandardMaskManyItem)
  public
    function Check(var AChar: Char): Boolean; override;
  end;

  { TcxStandardMaskNumericSymbolItem }

  TcxStandardMaskNumericSymbolItem = class(TcxStandardMaskManyItem)
  public
    constructor Create(AOptional: Boolean; ACaseControl: TcxCaseControl);
    function Check(var AChar: Char): Boolean; override;
  end;

  { TcxStandardMask }

  TcxStandardMask = class
  private
    FBlank: Char;
    FItems: TList;
    FLeading: Boolean;
    FSaveLiteralCharacters: Boolean;
    FMask: string;
    procedure Clear;
    procedure DoCompileBody(const AMask: string);
    procedure DoCompileHead(var AMask: string);
    function GetCount: Integer;
    function GetEmptyString: string;
    function GetFullEmptyString: string;
    function GetItems(AIndex: Integer): TcxStandardMaskCustomItem;
    function IsBlank(AChar: Char): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(AMask: string);
    procedure Format(var AText: string; AChangeCharCase: Boolean = True;
      AMatchForBlanksAndLiterals: Boolean = True);
    procedure Format2(var AText: string);
    function IsFullValid(var AText: string): Boolean;
    function IsValid(var AText: string): Boolean;
    property Blank: Char read FBlank write FBlank;
    property Count: Integer read GetCount;
    property EmptyString: string read GetEmptyString;
    property FullEmptyString: string read GetFullEmptyString;
    property Items[AIndex: Integer]: TcxStandardMaskCustomItem read GetItems;
    property Mask: string read FMask;
    property SaveLiteralCharacters: Boolean read FSaveLiteralCharacters;
  end;

  function EmptyString(const AMask: string; const ABlank: Char = #0): string;
  function FormatText(const AText, AMask: string; const ABlank: Char = #0): string;
  function IsTextFullValid(const AText, AMask: string): Boolean;
  function IsTextValid(const AText, AMask: string): Boolean;
  function MaskBlank(const AMask: string): Char;
  function SaveLiteralChars(const AMask: string): Boolean;

const
  cxDefaultBlank = '_';

implementation

uses
  dxCore, cxFormats;

function EmptyString(const AMask: string; const ABlank: Char): string;
var
  AMaskObject: TcxStandardMask;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    if ABlank <> #0 then
      AMaskObject.Blank := ABlank;
    Result := AMaskObject.EmptyString;
  finally
    AMaskObject.Free;
  end;
end;

function FormatText(const AText, AMask: string; const ABlank: Char): string;
var
  AMaskObject: TcxStandardMask;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    Result := AText;
    if ABlank <> #0 then
      AMaskObject.Blank := ABlank;
    AMaskObject.Format(Result);
  finally
    AMaskObject.Free;
  end;
end;

function IsTextFullValid(const AText, AMask: string): Boolean;
var
  AMaskObject: TcxStandardMask;
  AAText: string;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    AAText := AText;
    Result := AMaskObject.IsFullValid(AAText);
  finally
    AMaskObject.Free;
  end;
end;

function IsTextValid(const AText, AMask: string): Boolean;
var
  AMaskObject: TcxStandardMask;
  AAText: string;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    AAText := AText;
    Result := AMaskObject.IsValid(AAText);
  finally
    AMaskObject.Free;
  end;
end;

function MaskBlank(const AMask: string): Char;
var
  AMaskObject: TcxStandardMask;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    Result := AMaskObject.Blank;
  finally
    AMaskObject.Free;
  end;
end;

function SaveLiteralChars(const AMask: string): Boolean;
var
  AMaskObject: TcxStandardMask;
begin
  AMaskObject := TcxStandardMask.Create;
  try
    AMaskObject.Compile(AMask);
    Result := AMaskObject.SaveLiteralCharacters;
  finally
    AMaskObject.Free;
  end;
end;

{ TcxStandardMaskLiteralItem }

constructor TcxStandardMaskLiteralItem.Create(ALiteral: Char);
begin
  inherited Create;
  FLiteral := ALiteral;
end;

function TcxStandardMaskLiteralItem.Check(var AChar: Char): Boolean;
begin
  Result := AChar = FLiteral;
end;

{ TcxStandardMaskManyItem }

constructor TcxStandardMaskManyItem.Create(AOptional: Boolean; ACaseControl: TcxCaseControl);
begin
  inherited Create;
  FOptional := AOptional;
  FCaseControl := ACaseControl;
end;

procedure TcxStandardMaskManyItem.DoCaseControl(var AChar: Char);
var
  AStr: string;
begin
  AStr := AChar;

  case FCaseControl of
    ccUpperCase:
      AStr := AnsiUpperCase(AStr);
    ccLowerCase:
     AStr := AnsiLowerCase(AStr);
    ccUsercase:;
  end;

  AChar := AStr[1];
end;

{ TcxStandardMaskAlphaItem }

function TcxStandardMaskAlphaItem.Check(var AChar: Char): Boolean;
begin
  Result := dxCharIsAlpha(AChar);
  DoCaseControl(AChar);
end;

{ TcxStandardMaskAlphaNumericItem }

function TcxStandardMaskAlphaNumericItem.Check(var AChar: Char): Boolean;
begin
  Result := dxCharIsNumeric(AChar) or dxCharIsAlpha(AChar);
  DoCaseControl(AChar);
end;

{ TcxStandardMaskASCIIItem }

function TcxStandardMaskASCIIItem.Check(var AChar: Char): Boolean;
begin
  Result := True;
  DoCaseControl(AChar);
end;

{ TcxStandardMaskNumericItem }

function TcxStandardMaskNumericItem.Check(var AChar: Char): Boolean;
begin
  Result := dxCharIsNumeric(AChar);
  DoCaseControl(AChar);
end;

{ TcxStandardMaskNumericSymbolItem }

constructor TcxStandardMaskNumericSymbolItem.Create(AOptional: Boolean; ACaseControl: TcxCaseControl);
begin
  inherited Create(True, ACaseControl);
end;

function TcxStandardMaskNumericSymbolItem.Check(var AChar: Char): Boolean;
begin
  Result := dxCharIsNumeric(AChar) or dxCharInSet(AChar, ['+', '-']);
  DoCaseControl(AChar);
end;

{ TcxStandardMask }

constructor TcxStandardMask.Create;
begin
  inherited Create;
  FMask := '';
  FLeading := False;
  FSaveLiteralCharacters := True;
  FBlank := cxDefaultBlank;
  FItems := TList.Create;
end;

destructor TcxStandardMask.Destroy;
begin
  Clear;
  FItems.Free;

  inherited Destroy;
end;

procedure TcxStandardMask.Compile(AMask: string);
var
  AString: string;
begin
  Clear;

  FMask := AMask;
  AString := AMask;
  DoCompileHead(AString);
  DoCompileBody(AString);
end;

procedure TcxStandardMask.Format(var AText: string; AChangeCharCase: Boolean = True;
  AMatchForBlanksAndLiterals: Boolean = True);

  procedure InitializeVariables(out S: string; out ANotTestedPos, APos, AStep: Integer);
  begin
    S := FullEmptyString;

    if FLeading then
    begin
      ANotTestedPos := Count - 1;
      AStep := -1;
      APos := Length(AText);
    end
    else
    begin
      ANotTestedPos := 0;
      AStep := 1;
      APos := 1;
    end;
  end;

var
  AChar: Char;
  ACheckLiteralItem: Boolean;
  ANotTestedPos, AStep, I, J: Integer;
  AResult: string;
  AIsBlank: Boolean;
begin
  InitializeVariables(AResult, ANotTestedPos, I, AStep);
  ACheckLiteralItem := not SaveLiteralCharacters and not AMatchForBlanksAndLiterals and
    (Length(AResult) <> Length(AText));

  while (I >= 1) and (I <= Length(AText)) do
  begin
    AChar := AText[I];
    AIsBlank := IsBlank(AChar);
    J := ANotTestedPos;
    while (J >= 0) and (J < Count) do
    begin
      if ACheckLiteralItem and (Items[J] is TcxStandardMaskLiteralItem) then
      begin
        Inc(J, AStep);
        Continue;
      end;
      if Items[J].Check(AChar) then
      begin
        Delete(AResult, J + 1, 1);
        if AChangeCharCase then
          Insert(AChar, AResult, J + 1)
        else
          Insert(AText[I], AResult, J + 1);
        ANotTestedPos := J + AStep;
        Break;
      end
      else
      begin
        if AIsBlank and (Items[J] is TcxStandardMaskManyItem) then
        begin
          ANotTestedPos := J + AStep;
          Break;
        end;
      end;
      Inc(J, AStep);
    end;
    Inc(I, AStep);
  end;

  AText := AResult;
end;

// The AText must be fotmatted by Format procedure already
procedure TcxStandardMask.Format2(var AText: string);

  function FormatWithLiteralCharacters: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(AText) do
    begin
      if I > Count then
        Break;
      if Items[I - 1] is TcxStandardMaskLiteralItem then
        Result := Result + AText[I]
      else if Items[I - 1] is TcxStandardMaskManyItem then
      begin
        if AText[I] = FBlank then
          Result := Result + ' '
        else
          Result := Result + AText[I];
      end;
    end;
  end;

  function FormatWithoutLiteralCharacters: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(AText) do
    begin
      if I > Count then
        Break;
      if Items[I - 1] is TcxStandardMaskManyItem then
        if AText[I] = FBlank then
          Result := Result + ' '
        else
          Result := Result + AText[I];
    end;
  end;

begin
  if SaveLiteralCharacters then
    AText := FormatWithLiteralCharacters
  else
    AText := FormatWithoutLiteralCharacters;
end;

function TcxStandardMask.IsBlank(AChar: Char): Boolean;
begin
  Result := (AChar = FBlank) or (AChar = ' ');
end;

function TcxStandardMask.IsFullValid(var AText: string): Boolean;
var
  AIsCharValid: Boolean;
  I: Integer;
begin
  if Length(AText) = Count then
  begin
    Result := True;
    for I := 1 to Length(AText) do
    begin
      AIsCharValid := Items[I - 1].Check(AText[I]);
      if not AIsCharValid then
      begin
        if IsBlank(AText[I]) and (Items[I - 1] is TcxStandardMaskManyItem) and
            (TcxStandardMaskManyItem(Items[I - 1]).Optional) then
          Continue
        else
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end
  else
    Result := False;
end;

function TcxStandardMask.IsValid(var AText: string): Boolean;
var
  AIsCharValid: Boolean;
  I: Integer;
begin
  if Length(AText) <= Count then
  begin
    Result := True;
    for I := 1 to Length(AText) do
    begin
      AIsCharValid := Items[I - 1].Check(AText[I]);
      if not AIsCharValid then
      begin
        if IsBlank(AText[I]) and (Items[I - 1] is TcxStandardMaskManyItem) then
          Continue
        else
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end
  else
    Result := False;
end;

procedure TcxStandardMask.Clear;
var
  I: Integer;
begin
  FMask := '';
  FLeading := False;
  FSaveLiteralCharacters := True;
  FBlank := cxDefaultBlank;
  for I := 0 to FItems.Count - 1 do
    TcxStandardMaskCustomItem(FItems[I]).Free;
  FItems.Clear;
end;

procedure TcxStandardMask.DoCompileBody(const AMask: string);
var
  I: Integer;
  ACaseControl: TcxCaseControl;
begin
  I := 1;
  ACaseControl := ccUserCase;
  while I <= Length(AMask) do
  begin
    case AMask[I] of
      'L':
        begin
          FItems.Add(TcxStandardMaskAlphaItem.Create(False, ACaseControl));
          Inc(I);
        end;
      'l':
        begin
          FItems.Add(TcxStandardMaskAlphaItem.Create(True, ACaseControl));
          Inc(I);
        end;
      'A':
        begin
          FItems.Add(TcxStandardMaskAlphaNumericItem.Create(False, ACaseControl));
          Inc(I);
        end;
      'a':
        begin
          FItems.Add(TcxStandardMaskAlphaNumericItem.Create(True, ACaseControl));
          Inc(I);
        end;
      'C':
        begin
          FItems.Add(TcxStandardMaskASCIIItem.Create(False, ACaseControl));
          Inc(I);
        end;
      'c':
        begin
          FItems.Add(TcxStandardMaskASCIIItem.Create(True, ACaseControl));
          Inc(I);
        end;
      '0':
        begin
          FItems.Add(TcxStandardMaskNumericItem.Create(False, ACaseControl));
          Inc(I);
        end;
      '9':
        begin
          FItems.Add(TcxStandardMaskNumericItem.Create(True, ACaseControl));
          Inc(I);
        end;
      '#':
        begin
          FItems.Add(TcxStandardMaskNumericSymbolItem.Create(True, ACaseControl));
          Inc(I);
        end;
      ':':
        begin
          FItems.Add(TcxStandardMaskLiteralItem.Create(dxFormatSettings.TimeSeparator));
          Inc(I);
        end;
      '/':
        begin
          FItems.Add(TcxStandardMaskLiteralItem.Create(dxFormatSettings.DateSeparator));
          Inc(I);
        end;
      '\':
        begin
          Inc(I);
          if I <= Length(AMask) then
          begin
            FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
            Inc(I);
          end;
        end;
      '<':
        begin
          ACaseControl := ccLowerCase;
          Inc(I);
          if I <= Length(AMask) then
            if AMask[I] = '>' then
            begin
              ACaseControl := ccUserCase;
              Inc(I);
            end;
        end;
      '>':
        begin
          ACaseControl := ccUpperCase;
          Inc(I);
        end;
      ';':
        begin
          if Length(AMask) - I = 3 then
          begin
            if AMask[I + 2] = ';' then
            begin
              if (AMask[I + 1] = '0') or (AMask[I + 1] = '1') then
              begin
                FSaveLiteralCharacters := AMask[I + 1] <> '0';
                FBlank := AMask[I + 3];
                Inc(I, 4);
              end
              else
              begin
                FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
                Inc(I);
              end;
            end
            else
            begin
              FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
              Inc(I);
            end;
          end
          else if Length(AMask) - I = 1 then
          begin
            if (AMask[I + 1] = '0') or (AMask[I + 1] = '1') then
            begin
              FSaveLiteralCharacters := AMask[I + 1] <> '0';
              Inc(I, 2);
            end
            else
            begin
              FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
              Inc(I);
            end;
          end
          else
          begin
            FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
            Inc(I);
          end;
        end;
      else
      begin
        FItems.Add(TcxStandardMaskLiteralItem.Create(AMask[I]));
        Inc(I);
      end;
    end;
  end;
end;

procedure TcxStandardMask.DoCompileHead(var AMask: string);
begin
  if AMask <> '' then
  begin
    if AMask[1] = '!' then
    begin
      FLeading := True;
      Delete(AMask, 1, 1);
    end
    else
      FLeading := False;
  end;
end;

function TcxStandardMask.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxStandardMask.GetEmptyString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if Items[I] is TcxStandardMaskLiteralItem then
    begin
      if FSaveLiteralCharacters then
        Result := Result + TcxStandardMaskLiteralItem(Items[I]).Literal
    end
    else
      Result := Result + ' ';
  end;
end;

function TcxStandardMask.GetFullEmptyString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if Items[I] is TcxStandardMaskLiteralItem then
      Result := Result + TcxStandardMaskLiteralItem(Items[I]).Literal
    else if Items[I] is TcxStandardMaskManyItem then
      Result := Result + FBlank
  end;
end;

function TcxStandardMask.GetItems(AIndex: Integer): TcxStandardMaskCustomItem;
begin
  Result := TcxStandardMaskCustomItem(FItems[AIndex]);
end;

end.

