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

unit dxRichEdit.Import.CSSSelectorParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, RegularExpressions, Generics.Defaults, Generics.Collections,
  dxGenerics;

type

  { TdxCombinator }

  TdxCombinator = (
    None,
    WhiteSpace,
    RightAngle,
    PlusSign
  );

  { TdxSelectorParserState }

  TdxSelectorParserState = (
    StartState,
    ReadSelectorName,
    ReadClassName,
    ReadId,
    SkipWord,
    AutoSpace,
    ReadChildElement,
    ReadAttribute,
    ReadPseudoClass
  );

 { TdxAttributePattern }

  TdxAttributePattern = class
  private class var
    FPattern: string;
    FRegex: TRegex;
    class constructor Initialize;
  public
    class property RegEx: TRegex read FRegex;
    class function TryGetGroupValue(const AMatch: TMatch; const AGroupName: string): string; static;
  end;

  { TdxStyleClasses }

  TdxStyleClasses = TdxStringList;

  { TdxSelectorAttribute }

  TdxSelectorAttribute = class
  strict private
    FAttributeName: string;
    FAttributeValue: string;
    FAttributeConnector: string;
  public
    constructor Create;

    property AttributeName: string read FAttributeName write FAttributeName;
    property AttributeValue: string read FAttributeValue write FAttributeValue;
    property AttributeConnector: string read FAttributeConnector write FAttributeConnector;
  end;

  { TdxSimpleSelector }

  TdxSimpleSelector = class
  strict private
    FName: string;
    FClasses: TdxStyleClasses;
    FPseudoClasses: TdxStyleClasses;
    FId: string;
    FSelectorAttributes: TdxObjectList<TdxSelectorAttribute>;
  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Classes: TdxStyleClasses read FClasses write FClasses;
    property PseudoClasses: TdxStyleClasses read FPseudoClasses write FPseudoClasses;
    property Id: string read FId write FId;
    property SelectorAttributes: TdxObjectList<TdxSelectorAttribute> read FSelectorAttributes write FSelectorAttributes;
  end;

  { TdxSelectorElement }

  TdxSelectorElement = class
  strict private
    FSimpleSelector: TdxSimpleSelector;
    FCombinator: TdxCombinator;
  public
    constructor Create;
    destructor Destroy; override;

    property SimpleSelector: TdxSimpleSelector read FSimpleSelector write FSimpleSelector;
    property Combinator: TdxCombinator read FCombinator write FCombinator;
  end;
  TdxSelectorElementList = class(TdxObjectList<TdxSelectorElement>);

  { TdxSelector }

  TdxSelector = class
  strict private
    FSpecificity: Integer;
    FElements: TdxSelectorElementList;
    function GetSpecifity: Integer;
  protected
    function CalculateSpecifityCore(ASpecificity: Integer): Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidateSpecifity;

    property Elements: TdxSelectorElementList read FElements;
    property Specifity: Integer read GetSpecifity;
  end;
  TdxSelectorList = class(TdxList<TdxSelector>);

  { TdxSelectorParser }

  TdxSelectorParser = class
  strict private
    FRawText: string;
    FClassName: string;
    FPseudoClassName: string;
    FAttribute: string;
    FId: string;
    FName: TStringBuilder;
    FSelector: TdxSelector;
    FSelectors: TdxSelectorList;
    FState: TdxSelectorParserState;
    FElement: TdxSelectorElement;
  protected
    procedure AddSelector;
    procedure AddElementCore;
    procedure StartState(ACh: Char);
    procedure ReadSelectorName(ACh: Char);
    function IsReadSelectorName(ACh: Char): Boolean;
    function IsStartNewElement(ACh: Char): Boolean;
    procedure ReadId(ACh: Char);
    procedure ReadClassName(ACh: Char);
    procedure AutoSpace(ACh: Char);
    procedure SkipWord(ACh: Char);
    procedure ReadPseudoClass(ACh: Char);
    procedure AddElement(ACh: Char; ACombinator: TdxCombinator);
    procedure ReadAttribute(ACh: Char);
  public
    constructor Create(const ARawText: string);
    destructor Destroy; override;

    function Parse: TdxSelectorList; virtual;
  end;

implementation

uses
  Contnrs, Math, Character;

const
  AttrNamePattern = '(?<attrName>\w*)';
  ValuePattern = '"?'#$27'?(?<attrValue>[^"'#$27']*)"?'#$27'?';
  EqPattern = '(?<attrEq>=|\^=|\$=|~=|\*=|\|=)';
  SpacePattern = '\s*';

{ TdxAttributePattern }

class constructor TdxAttributePattern.Initialize;
begin
  FPattern := Format('%0:s%1:s%0:s(%2:s%0:s%3:s%0:s)?', [SpacePattern, AttrNamePattern, EqPattern, ValuePattern]);
  FRegex := TRegex.Create(FPattern, [roCompiled, roExplicitCapture]);
end;

class function TdxAttributePattern.TryGetGroupValue(const AMatch: TMatch; const AGroupName: string): string;
begin
{$IFNDEF DELPHIXE2}
  Result := '';
  if SameStr('attrName', AGroupName) then
    Result := AMatch.Groups[1].Value
  else
    if AMatch.Groups.Count > 2 then
      if SameStr('attrEq', AGroupName) then
        Result := AMatch.Groups[2].Value
      else
        if SameStr('attrValue', AGroupName) then
          Result := AMatch.Groups[3].Value;
{$ELSE}
  try
    Result := AMatch.Groups[AGroupName].Value;
  except
    Result := '';
  end;
{$ENDIF}
end;

{ TdxSelectorAttribute }

constructor TdxSelectorAttribute.Create;
begin
  FAttributeName := '';
  FAttributeValue := '';
  FAttributeConnector := '';
end;

{ TdxSimpleSelector }

constructor TdxSimpleSelector.Create;
begin
  inherited Create;
  FName := '';
  FClasses := TdxStyleClasses.Create;
  FPseudoClasses := TdxStyleClasses.Create;
  FId := '';
  FSelectorAttributes := TdxObjectList<TdxSelectorAttribute>.Create;
end;

destructor TdxSimpleSelector.Destroy;
begin
  FClasses.Free;
  FPseudoClasses.Free;
  FSelectorAttributes.Free;
  inherited Destroy;
end;

{ TdxSelectorElement }

constructor TdxSelectorElement.Create;
begin
  inherited Create;
  FSimpleSelector := TdxSimpleSelector.Create;
end;


destructor TdxSelectorElement.Destroy;
begin
  FSimpleSelector.Free;
  inherited Destroy;
end;

{ TdxSelector }

constructor TdxSelector.Create;
begin
  inherited Create;
  FSpecificity := -1;
  FElements := TdxSelectorElementList.Create;
end;

destructor TdxSelector.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TdxSelector.GetSpecifity: Integer;
begin
  if FSpecificity = -1 then
    FSpecificity := CalculateSpecifityCore(FSpecificity);
  Result := FSpecificity;
end;

procedure TdxSelector.InvalidateSpecifity;
begin
  FSpecificity := -1;
end;

function TdxSelector.CalculateSpecifityCore(ASpecificity: Integer): Integer;
var
  ACount, AIdCount, AAttributesCount, AElementsCount, I: Integer;
  ASimpleSelector: TdxSimpleSelector;
begin
  ACount := FElements.Count;
  AIdCount := 0;
  AAttributesCount := 0;
  AElementsCount := 0;
  for I := 0 to ACount - 1 do
  begin
    ASimpleSelector := FElements[I].SimpleSelector;
    if ASimpleSelector.Id <> '' then
      Inc(AIdCount);
    Inc(AAttributesCount, ASimpleSelector.SelectorAttributes.Count);
    Inc(AAttributesCount, ASimpleSelector.PseudoClasses.Count);
    Inc(AAttributesCount, ASimpleSelector.Classes.Count);
    if ASimpleSelector.Name <> '' then
      Inc(AElementsCount);
  end;
  AIdCount := Min(255, AIdCount);
  AAttributesCount := Min(255, AAttributesCount);
  AElementsCount := Min(2555, AElementsCount);
  Result := (AIdCount shl 16) + (AAttributesCount shl 8) + AElementsCount;
end;

{ TdxSelectorParser }

constructor TdxSelectorParser.Create(const ARawText: string);
begin
  inherited Create;
  FRawText := ARawText;
  FSelectors := TdxSelectorList.Create;
  FClassName := '';
  FPseudoClassName := '';
  FAttribute := '';
  FId := '';
  FSelector := TdxSelector.Create;
  FElement := TdxSelectorElement.Create;
  FName := TStringBuilder.Create;
end;

destructor TdxSelectorParser.Destroy;
begin
  FSelectors.Free;
  FSelector.Free;
  FElement.Free;
  FName.Free;
  inherited Destroy;
end;

function TdxSelectorParser.Parse: TdxSelectorList;
var
  ACh: Char;
begin
  FRawText := Trim(FRawText);
  if FRawText = '' then
  begin
    AddElementCore;
    FSelectors.Add(FSelector);
    FSelector := TdxSelector.Create;
  end
  else
  begin
    for ACh in FRawText do
      case FState of
        TdxSelectorParserState.StartState:
          StartState(ACh);
        TdxSelectorParserState.ReadSelectorName:
          ReadSelectorName(ACh);
        TdxSelectorParserState.ReadId:
          ReadId(ACh);
        TdxSelectorParserState.ReadClassName:
          ReadClassName(ACh);
        TdxSelectorParserState.AutoSpace:
          AutoSpace(ACh);
        TdxSelectorParserState.SkipWord:
          SkipWord(ACh);
        TdxSelectorParserState.ReadPseudoClass:
          ReadPseudoClass(ACh);
        TdxSelectorParserState.ReadAttribute:
          ReadAttribute(ACh);
      end;
    if FState <> TdxSelectorParserState.StartState then
      AddSelector;
  end;
  Result := FSelectors;
end;

procedure TdxSelectorParser.AddSelector;
begin
  if FClassName <> '' then
  begin
    FElement.SimpleSelector.Classes.Add(FClassName);
    FClassName := '';
  end;
  if FPseudoClassName <> '' then
  begin
    FElement.SimpleSelector.PseudoClasses.Add(FPseudoClassName);
    FPseudoClassName := '';
  end;
  if FId <> '' then
  begin
    FElement.SimpleSelector.Id := UpperCase(FId);
    FId := '';
  end;
  AddElementCore;
  FSelectors.Add(FSelector);
  FSelector := TdxSelector.Create;
end;

procedure TdxSelectorParser.AddElementCore;
begin
  FElement.SimpleSelector.Name := UpperCase(FName.ToString);
  FName.Length := 0;
  FSelector.Elements.Add(FElement);
  FElement := TdxSelectorElement.Create;
end;

procedure TdxSelectorParser.StartState(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
    Exit;
  if not IsReadSelectorName(ACh) then
  begin
  {$IFDEF DELPHIXE4}
    if ACh.IsLetter or (ACh = '*') or (ACh = '@') then
  {$ELSE}
    if TCharacter.IsLetter(ACh) or (ACh = '*') or (ACh = '@') then
  {$ENDIF}
    begin
      FName.Append(ACh);
      FState := TdxSelectorParserState.ReadSelectorName;
    end
    else
      FState := TdxSelectorParserState.SkipWord;
  end;
end;

procedure TdxSelectorParser.ReadSelectorName(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
  begin
    FState := TdxSelectorParserState.AutoSpace;
    Exit;
  end;
  if not IsReadSelectorName(ACh) and not IsStartNewElement(ACh) then
    FName.Append(ACh);
end;

function TdxSelectorParser.IsReadSelectorName(ACh: Char): Boolean;
begin
  case ACh of
    ':':
      FState := TdxSelectorParserState.ReadPseudoClass;
    '[':
      FState := TdxSelectorParserState.ReadAttribute;
    '.':
      FState := TdxSelectorParserState.ReadClassName;
    '#':
      FState := TdxSelectorParserState.ReadId;
    else
      Exit(False);
  end;
  Result := True;
end;

function TdxSelectorParser.IsStartNewElement(ACh: Char): Boolean;
begin
  case ACh of
    '+':
      AddElement(ACh, TdxCombinator.PlusSign);
    '>':
      AddElement(ACh, TdxCombinator.RightAngle);
    ',':
      begin
        AddSelector;
        FState := TdxSelectorParserState.StartState;
      end;
    else
      Exit(False);
  end;
  Result := True;
end;

procedure TdxSelectorParser.ReadId(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
  begin
    FElement.SimpleSelector.Id := UpperCase(FId);
    FId := '';
    FState := TdxSelectorParserState.AutoSpace;
    Exit;
  end;
  if ACh = ',' then
  begin
    AddSelector;
    FState := TdxSelectorParserState.StartState;
    Exit;
  end;
  FId := FId + ACh;
end;

procedure TdxSelectorParser.ReadClassName(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
  begin
    FElement.SimpleSelector.Classes.Add(FClassName);
    FClassName := '';
    FState := TdxSelectorParserState.AutoSpace;
  end
  else
    if ACh = ',' then
    begin
      AddSelector;
      FState := TdxSelectorParserState.StartState;
    end
    else
      if ACh = '.' then
      begin
        if FClassName <> '' then
          FElement.SimpleSelector.Classes.Add(FClassName);
        FClassName := '';
      end
      else
        FClassName := FClassName + ACh;
end;

procedure TdxSelectorParser.AutoSpace(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
    Exit;
  if not IsStartNewElement(ACh) then
  begin
    AddElement(ACh, TdxCombinator.WhiteSpace);
    FState := TdxSelectorParserState.ReadSelectorName;
    ReadSelectorName(ACh);
  end;
end;

procedure TdxSelectorParser.SkipWord(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace or (ACh = ',') then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) or (ACh = ',') then
{$ENDIF}
    FState := TdxSelectorParserState.StartState;
end;

procedure TdxSelectorParser.ReadPseudoClass(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
  begin
    FElement.SimpleSelector.PseudoClasses.Add(FPseudoClassName);
    FPseudoClassName := '';
    FState := TdxSelectorParserState.AutoSpace;
    Exit;
  end;
  if ACh = ':' then
  begin
    if FPseudoClassName = '' then
      FElement.SimpleSelector.PseudoClasses.Add(FPseudoClassName);
    FPseudoClassName := '';
    Exit;
  end;
  if ACh = ',' then
  begin
    AddSelector;
    FState := TdxSelectorParserState.StartState;
    Exit;
  end;
  FPseudoClassName := FPseudoClassName + ACh;
end;

procedure TdxSelectorParser.AddElement(ACh: Char; ACombinator: TdxCombinator);
begin
  AddElementCore;
  FElement.Combinator := ACombinator;
  FState := TdxSelectorParserState.StartState;
end;

procedure TdxSelectorParser.ReadAttribute(ACh: Char);
var
  AMatch: TMatch;
  ASelectorAttribute: TdxSelectorAttribute;
begin
  if ACh = ']' then
  begin
    AMatch := TdxAttributePattern.Regex.Match(FAttribute);
    ASelectorAttribute := TdxSelectorAttribute.Create;
    ASelectorAttribute.AttributeName := UpperCase(AMatch.Groups[1].Value);
    if AMatch.Groups.Count > 2 then
    begin
      ASelectorAttribute.AttributeConnector := AMatch.Groups[2].Value;
      ASelectorAttribute.AttributeValue := AMatch.Groups[3].Value;
    end;
    FElement.SimpleSelector.SelectorAttributes.Add(ASelectorAttribute);
    FAttribute := '';
    FState := TdxSelectorParserState.ReadSelectorName;
  end
  else
    FAttribute := FAttribute + ACh;
end;

end.
