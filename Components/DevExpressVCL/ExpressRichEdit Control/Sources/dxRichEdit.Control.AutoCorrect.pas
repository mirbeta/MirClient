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

unit dxRichEdit.Control.AutoCorrect;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Character, Generics.Defaults, Generics.Collections, Classes, Controls, Rtti,
  dxSpellCheckerCore,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.InnerControl,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.Hyperlink;

type

  { TdxAutoCorrectInfo }

  TdxAutoCorrectInfo = record
  strict private
    FReplaceWith: TValue;
    FStart: TdxDocumentModelPosition;
    FEnd: TdxDocumentModelPosition;
    FText: string;
    FDocumentServer: TdxInnerRichEditDocumentServer;
    FRule: IdxSpellCheckerAutoCorrectCustomRule;
    FIsNull: Boolean;
    function CreateIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
    function GetPieceTable: TdxPieceTable;
    function GetCanDecrementStart: Boolean;
    class function GetNull: TdxAutoCorrectInfo; static;
  public
    constructor Create(const ADocumentServer: TdxInnerRichEditDocumentServer);

    procedure DecrementEndPosition;
    procedure IncrementEndPosition;
    function DecrementStartPosition: Boolean;
    function IncrementStartPosition: Boolean;
    function IsNull: Boolean;

    property Rule: IdxSpellCheckerAutoCorrectCustomRule read FRule write FRule;
    property Start: TdxDocumentModelPosition read FStart;
    property &End: TdxDocumentModelPosition read FEnd;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property DocumentServer: TdxInnerRichEditDocumentServer read FDocumentServer;
    property CanDecrementStart: Boolean read GetCanDecrementStart;
    property ReplaceWith: TValue read FReplaceWith write FReplaceWith;
    property Text: string read FText;

    class property Null: TdxAutoCorrectInfo read GetNull;
  end;

  { IdxAutoCorrectProvider }

  IdxAutoCorrectProvider = interface
    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
  end;
  TdxAutoCorrectProviderCollection = TList<IdxAutoCorrectProvider>;

  { IdxAutoCorrectService }

  IdxAutoCorrectService = interface
  ['{A978C701-8162-4110-8F01-0E360A761359}']
    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
    procedure ApplyAutoCorrectInfo(const AInfo: TdxAutoCorrectInfo);
    procedure RegisterProvider(const AProvider: IdxAutoCorrectProvider);
    procedure UnregisterProvider(const AProvider: IdxAutoCorrectProvider);
  end;

  { TdxAutoCorrectService }

  TdxAutoCorrectService = class(TInterfacedObject, IdxAutoCorrectService)
  strict private
    FDocumentServer: TdxInnerRichEditDocumentServer;
    FProviders: TdxAutoCorrectProviderCollection;
    function GetDocumentModel: TdxDocumentModel;
  protected
    procedure RegisterDefaultProviders; virtual;
    procedure ReplaceWithText(const AInfo: TdxAutoCorrectInfo; const AReplaceString: string);
    procedure ReplaceWithTextCore(const AInfo: TdxAutoCorrectInfo; const AReplaceString: string; ALength: Integer);
    procedure UpdateCaretPositionX(const AInfo: TdxAutoCorrectInfo);
    procedure ReplaceWithHyperlink(const AInfo: TdxAutoCorrectInfo; const AHyperlinkInfo: TdxHyperlinkInfo);
    procedure ReplaceWithHyperlinkCore(const AInfo: TdxAutoCorrectInfo; const AHyperlinkInfo: TdxHyperlinkInfo; ALength: Integer);
    procedure ReplaceWithPicture(const AInfo: TdxAutoCorrectInfo; AImage: TdxOfficeImageReference);
    procedure ReplaceWithPictureCore(const AInfo: TdxAutoCorrectInfo;
      AImage: TdxOfficeImageReference; ALength: Integer);
  public
    constructor Create(const ADocumentServer: TdxInnerRichEditDocumentServer);
    destructor Destroy; override;

    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
    procedure ApplyAutoCorrectInfo(const AInfo: TdxAutoCorrectInfo);
    procedure RegisterProvider(const AProvider: IdxAutoCorrectProvider);
    procedure InnerRegisterProvider(const AProvider: IdxAutoCorrectProvider);
    procedure UnregisterProvider(const AProvider: IdxAutoCorrectProvider);

    property DocumentServer: TdxInnerRichEditDocumentServer read FDocumentServer;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Providers: TdxAutoCorrectProviderCollection read FProviders;
  end;

  { TdxConditionalWordReplaceAutoCorrectProvider }

  TdxConditionalWordReplaceAutoCorrectProvider = class abstract(TInterfacedObject, IdxAutoCorrectProvider)
  strict private
    FDocumentServer: TdxInnerRichEditDocumentServer;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function IsValidInitialText(const AText: string): Boolean; virtual;
    function BeforeFirstGetText(const AInfo: TdxAutoCorrectInfo): Boolean; virtual;
    function IsAutoCorrectAllowed(const AInfo: TdxAutoCorrectInfo): Boolean; virtual;
    function IsSeparator(ACh: Char): Boolean; virtual;
    function IsTriggerChar(ACh: Char): Boolean; virtual;
    function CalculateWordReplacement(const AInfo: TdxAutoCorrectInfo): TValue; virtual; abstract;
  public
    constructor Create(ADocumentServer: TdxInnerRichEditDocumentServer);
    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo; virtual;

    property DocumentServer: TdxInnerRichEditDocumentServer read FDocumentServer;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

  { TdxRichEditAutoCorrectHistoryItem }

  TdxRichEditAutoCorrectHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FRule: IdxSpellCheckerAutoCorrectCustomRule;
    FSpellChecker: IdxSpellChecker3;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable;
      const ARule: IdxSpellCheckerAutoCorrectCustomRule); reintroduce;
  end;

implementation

uses
  RegularExpressions,
  dxCore, dxCoreClasses, dxThreading,
  dxRichEdit.InnerControl.SpellCheckerController,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxStringHelper,
  dxCharacters,
  dxRichEdit.DocumentModel.Fields.Core;

type
  TdxTextRunBaseAccess = class(TdxTextRunBase);

  { TdxAutoCorrectRichEditTextController }

  TdxAutoCorrectRichEditTextController = class(TdxRichEditTextController)
  strict private
    FWordIterator: TdxSpellCheckerWordIterator;
  protected
    function GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator; override;
  public
    destructor Destroy; override;
    procedure Reset;
  end;

  { TdxUrlAutoCorrectProvider }

  TdxUrlAutoCorrectProvider = class(TdxConditionalWordReplaceAutoCorrectProvider)
  strict private
    class var
      FUrlRegex: TRegEx;
      FEmailRegex: TRegEx;
  strict private
    class constructor Initialize;
  protected
    function IsSeparator(ACh: Char): Boolean; override;
    function CalculateWordReplacement(const AInfo: TdxAutoCorrectInfo): TValue; override;
    function CreateNavigateUri(const AWord: string): string;
    function IsValidUrl(const AText: string): Boolean;
    function IsAutoCorrectAllowed(const AInfo: TdxAutoCorrectInfo): Boolean; override;
  public
    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo; override;
  end;

  { TdxSpellCheckerAutoCorrectProvider }

  TdxSpellCheckerAutoCorrectProvider = class(TdxConditionalWordReplaceAutoCorrectProvider)
  strict private
    FRule: IdxSpellCheckerAutoCorrectCustomRule;
  protected
    function CalculateWordReplacement(const AInfo: TdxAutoCorrectInfo): TValue; override;
  public
    function CalculateAutoCorrectInfo: TdxAutoCorrectInfo; override;
  end;

{ TdxAutoCorrectRichEditTextController }

destructor TdxAutoCorrectRichEditTextController.Destroy;
begin
  FreeAndNil(FWordIterator);
  inherited Destroy;
end;

procedure TdxAutoCorrectRichEditTextController.Reset;
begin
  FreeAndNil(FWordIterator);
end;

function TdxAutoCorrectRichEditTextController.GetWordIterator(APieceTable: TdxCustomPieceTable): TdxSpellCheckerWordIterator;
begin
  if (FWordIterator = nil) or (FWordIterator.PieceTable <> PieceTable) then
  begin
    FWordIterator.Free;
    FWordIterator := TdxSpellCheckerWordIterator.Create(PieceTable);
  end;
  Result := FWordIterator;
end;

{ TdxAutoCorrectInfo }

constructor TdxAutoCorrectInfo.Create(const ADocumentServer: TdxInnerRichEditDocumentServer);
begin
  FReplaceWith := '';
  FText := '';
  FIsNull := False;
  FDocumentServer := ADocumentServer;
  FEnd := ADocumentServer.DocumentModel.Selection.Interval.NormalizedEnd^;
  FStart := FEnd;
  DecrementStartPosition;
end;

function TdxAutoCorrectInfo.CreateIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
begin
  Result := TdxVisibleCharactersStopAtFieldsDocumentModelIterator.Create(PieceTable);
end;

function TdxAutoCorrectInfo.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FEnd.PieceTable);
end;

function TdxAutoCorrectInfo.GetCanDecrementStart: Boolean;
begin
  Result := Start.LogPosition <> 0;
end;

class function TdxAutoCorrectInfo.GetNull: TdxAutoCorrectInfo;
begin
  Result.FIsNull := True;
end;

procedure TdxAutoCorrectInfo.DecrementEndPosition;
var
  AIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
begin
  AIterator := CreateIterator;
  try
    FEnd := AIterator.MoveBack(FEnd);
  finally
    AIterator.Free;
  end;

  if Length(FText) >= 1 then
    Delete(FText, Length(FText), 1);
end;

procedure TdxAutoCorrectInfo.IncrementEndPosition;
var
  ARun: TdxTextRunBase;
  AValue: string;
  AIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
begin
  AIterator := CreateIterator;
  try
    FEnd := AIterator.MoveForward(FEnd);
  finally
    AIterator.Free;
  end;

  ARun := PieceTable.Runs[FEnd.RunIndex];
  AValue := TdxTextRunBaseAccess(ARun).GetPlainText(PieceTable.TextBuffer, FEnd.RunOffset, FEnd.RunOffset);
  if Length(AValue) > 1 then
    AValue := TdxStringHelper.Substring(AValue, 0, 1);
  FText := FText + AValue;
end;

function TdxAutoCorrectInfo.DecrementStartPosition: Boolean;
var
  APos: TdxDocumentLogPosition;
  ARun: TdxTextRunBase;
  AValue: string;
  AIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
begin
  APos := FStart.LogPosition;
  AIterator := CreateIterator;
  try
    FStart := AIterator.MoveBack(FStart);
  finally
    AIterator.Free;
  end;
  if APos > FStart.LogPosition then
  begin
    ARun := PieceTable.Runs[FStart.RunIndex];
    AValue := TdxTextRunBaseAccess(ARun).GetPlainText(PieceTable.TextBuffer, FStart.RunOffset, FStart.RunOffset);
    if Length(AValue) > 1 then
      AValue := TdxStringHelper.Substring(AValue, 0, 1);
    Insert(AValue, FText, 1);
    Result := True;
  end
  else
    Result := False;
end;

function TdxAutoCorrectInfo.IncrementStartPosition: Boolean;
var
  APos: TdxDocumentLogPosition;
  AIterator: TdxVisibleCharactersStopAtFieldsDocumentModelIterator;
begin
  APos := FStart.LogPosition;
  AIterator := CreateIterator;
  try
    FStart := AIterator.MoveForward(FStart);
  finally
    AIterator.Free;
  end;
  if APos < FStart.LogPosition then
  begin
    Delete(FText, 1, 1);
    Result := True;
  end
  else
    Result := False;
end;

function TdxAutoCorrectInfo.IsNull: Boolean;
begin
  Result := FIsNull;
end;

{ TdxAutoCorrectService }

constructor TdxAutoCorrectService.Create(const ADocumentServer: TdxInnerRichEditDocumentServer);
begin
  inherited Create;
  FDocumentServer := ADocumentServer;
  FProviders := TdxAutoCorrectProviderCollection.Create;
  RegisterDefaultProviders;
end;

destructor TdxAutoCorrectService.Destroy;
begin
  FreeAndNil(FProviders);
  inherited Destroy;
end;

function TdxAutoCorrectService.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocumentServer.DocumentModel;
end;

procedure TdxAutoCorrectService.RegisterDefaultProviders;
begin
  InnerRegisterProvider(TdxUrlAutoCorrectProvider.Create(DocumentServer));
  InnerRegisterProvider(TdxSpellCheckerAutoCorrectProvider.Create(DocumentServer));
end;

function TdxAutoCorrectService.CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
var
  ACount, I: Integer;
  AInfo: TdxAutoCorrectInfo;
begin
  ACount := Providers.Count;
  for I := 0 to ACount - 1 do
  begin
    try
      AInfo := Providers[I].CalculateAutoCorrectInfo;
      if not AInfo.IsNull then
        Exit(AInfo);
    except
    end;
  end;
  Result := TdxAutoCorrectInfo.Null;
end;

procedure TdxAutoCorrectService.ApplyAutoCorrectInfo(const AInfo: TdxAutoCorrectInfo);
var
  AHyperlinkInfo: TdxHyperlinkInfo;
  AReplaceString: string;
begin
  if AInfo.IsNull then
    Exit;

  if AInfo.ReplaceWith.IsType<TdxHyperlinkInfo> then
  begin
    AHyperlinkInfo := AInfo.ReplaceWith.AsType<TdxHyperlinkInfo>;
    ReplaceWithHyperlink(AInfo, AHyperlinkInfo);
    Exit;
  end;

  if AInfo.ReplaceWith.IsString then
  begin
    AReplaceString := AInfo.ReplaceWith.AsString;
    ReplaceWithText(AInfo, AReplaceString);
    Exit;
  end;
end;

procedure TdxAutoCorrectService.ReplaceWithText(const AInfo: TdxAutoCorrectInfo; const AReplaceString: string);
var
  ALength: Integer;
  AInnerInfo: TdxAutoCorrectInfo;
begin
  ALength := AInfo.&End.LogPosition - AInfo.Start.LogPosition;
  if ALength <= 0 then
    Exit;

  AInnerInfo := AInfo;
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(
    FDocumentServer,
    procedure
    begin
      ReplaceWithTextCore(AInnerInfo, AReplaceString, ALength);
    end);
end;

procedure TdxAutoCorrectService.ReplaceWithTextCore(const AInfo: TdxAutoCorrectInfo; const AReplaceString: string; ALength: Integer);
begin
  AInfo.DocumentServer.BeginUpdate;
  try
    AInfo.PieceTable.ApplyAutoCorrect(AInfo.Start.LogPosition, ALength, AReplaceString, AInfo.Rule);
    UpdateCaretPositionX(AInfo);
  finally
    AInfo.DocumentServer.EndUpdate;
  end;
end;

procedure TdxAutoCorrectService.UpdateCaretPositionX(const AInfo: TdxAutoCorrectInfo);
var
  AControl: TdxInnerRichEditControl;
  ACaretPosition: TdxCaretPosition;
begin
  AControl := Safe<TdxInnerRichEditControl>.Cast(AInfo.DocumentServer);
  if (AControl <> nil) and not AControl.IsDisposed then
  begin
    ACaretPosition := AControl.ActiveView.CaretPosition;
    ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Character);
    if ACaretPosition.LayoutPosition.Character <> nil then
      ACaretPosition.X := ACaretPosition.CalculateCaretBounds.Left;
  end;
end;

procedure TdxAutoCorrectService.ReplaceWithHyperlink(const AInfo: TdxAutoCorrectInfo;
  const AHyperlinkInfo: TdxHyperlinkInfo);
var
  ALength: Integer;
  AInnerInfo: TdxAutoCorrectInfo;
begin
  ALength := AInfo.&End.LogPosition - AInfo.Start.LogPosition;
  if ALength <= 0 then
    Exit;

  AInnerInfo := AInfo;
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(
    FDocumentServer,
    procedure
    begin
      ReplaceWithHyperlinkCore(AInnerInfo, AHyperlinkInfo, ALength);
    end);
end;

procedure TdxAutoCorrectService.ReplaceWithHyperlinkCore(const AInfo: TdxAutoCorrectInfo;
  const AHyperlinkInfo: TdxHyperlinkInfo; ALength: Integer);
begin
  AInfo.DocumentServer.BeginUpdate;
  try
    AInfo.PieceTable.CreateHyperlink(AInfo.Start.LogPosition, ALength, AHyperlinkInfo);
    UpdateCaretPositionX(AInfo);
  finally
    AInfo.DocumentServer.EndUpdate;
  end;
end;

procedure TdxAutoCorrectService.ReplaceWithPicture(const AInfo: TdxAutoCorrectInfo;
  AImage: TdxOfficeImageReference);
var
  ALength: Integer;
  AInnerInfo: TdxAutoCorrectInfo;
begin
  ALength := AInfo.&End.LogPosition - AInfo.Start.LogPosition;
  if ALength <= 0 then
    Exit;

  AInnerInfo := AInfo;
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(
    FDocumentServer,
    procedure
    begin
      ReplaceWithPictureCore(AInnerInfo, AImage, ALength);
    end);
end;

procedure TdxAutoCorrectService.ReplaceWithPictureCore(const AInfo: TdxAutoCorrectInfo;
  AImage: TdxOfficeImageReference; ALength: Integer);
begin
  AInfo.DocumentServer.BeginUpdate;
  try
    AInfo.PieceTable.ReplaceTextWithPicture(AInfo.Start.LogPosition, ALength, AImage);
    UpdateCaretPositionX(AInfo);
  finally
    AInfo.DocumentServer.EndUpdate;
  end;
end;

procedure TdxAutoCorrectService.RegisterProvider(const AProvider: IdxAutoCorrectProvider);
begin
  if AProvider = nil then
    Exit;

  Providers.Insert(0, AProvider);
end;

procedure TdxAutoCorrectService.InnerRegisterProvider(const AProvider: IdxAutoCorrectProvider);
begin
  if AProvider = nil then
    Exit;

  Providers.Add(AProvider);
end;

procedure TdxAutoCorrectService.UnregisterProvider(const AProvider: IdxAutoCorrectProvider);
var
  AIndex: Integer;
begin
  if AProvider = nil then
    Exit;

  AIndex := Providers.IndexOf(AProvider);
  if AIndex >= 0 then
    Providers.Delete(AIndex);
end;

{ TdxConditionalWordReplaceAutoCorrectProvider }

constructor TdxConditionalWordReplaceAutoCorrectProvider.Create(ADocumentServer: TdxInnerRichEditDocumentServer);
begin
  inherited Create;
  FDocumentServer := ADocumentServer;
end;

function TdxConditionalWordReplaceAutoCorrectProvider.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocumentServer.DocumentModel;
end;

function TdxConditionalWordReplaceAutoCorrectProvider.CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
var
  AInfo: TdxAutoCorrectInfo;
  AText: string;
  AIsSeparator: Boolean;
  AWith: TValue;
begin
  AInfo := TdxAutoCorrectInfo.Create(DocumentServer);

  if not IsAutoCorrectAllowed(AInfo) then
    Exit(TdxAutoCorrectInfo.Null);

  AText := AInfo.Text;
  if (AText = '') or (not IsTriggerChar(AText[1])) then
    Exit(TdxAutoCorrectInfo.Null);

  if not BeforeFirstGetText(AInfo) then
    Exit(TdxAutoCorrectInfo.Null);
  AText := AInfo.Text;
  if not IsValidInitialText(AText) then
    Exit(TdxAutoCorrectInfo.Null);

  while True do
  begin
    if not AInfo.DecrementStartPosition then
      Exit(TdxAutoCorrectInfo.Null);

    AText := AInfo.Text;
    AIsSeparator := IsSeparator(AText[1]);
    if not AInfo.CanDecrementStart or AIsSeparator then
    begin
      if AIsSeparator then
        AInfo.IncrementStartPosition;
      AWith := CalculateWordReplacement(AInfo);
      if not AWith.IsEmpty then
      begin
        AInfo.ReplaceWith := AWith;
        Exit(AInfo);
      end
      else
        Exit(TdxAutoCorrectInfo.Null);
    end;
  end;
end;

function TdxConditionalWordReplaceAutoCorrectProvider.IsValidInitialText(const AText: string): Boolean;
begin
  Result := (AText <> '') and not IsTriggerChar(AText[1]);
end;

function TdxConditionalWordReplaceAutoCorrectProvider.BeforeFirstGetText(const AInfo: TdxAutoCorrectInfo): Boolean;
begin
  AInfo.DecrementEndPosition;
  Result := AInfo.DecrementStartPosition;
end;

function TdxConditionalWordReplaceAutoCorrectProvider.IsAutoCorrectAllowed(const AInfo: TdxAutoCorrectInfo): Boolean;
begin
  Result := True;
end;

function TdxConditionalWordReplaceAutoCorrectProvider.IsSeparator(ACh: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := ACh.IsPunctuation or ACh.IsSeparator or ACh.IsWhiteSpace or
    CharInSet(ACh, [#13, #10, #$A0, TdxCharacters.PageBreak, TdxCharacters.ColumnBreak]);
{$ELSE}
  Result := IsPunctuation(ACh) or TCharacter.IsSeparator(ACh) or IsWhiteSpace(ACh) or
    CharInSet(ACh, [#13, #10, TdxCharacters.NonBreakingSpace, TdxCharacters.PageBreak, TdxCharacters.ColumnBreak]);
{$ENDIF}
end;

function TdxConditionalWordReplaceAutoCorrectProvider.IsTriggerChar(ACh: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := ACh.IsWhiteSpace or
    CharInSet(ACh, [#13, #10, #$A0, TdxCharacters.PageBreak, TdxCharacters.ColumnBreak]);
{$ELSE}
  Result := IsWhiteSpace(ACh) or
    CharInSet(ACh, [#13, #10, TdxCharacters.NonBreakingSpace, TdxCharacters.PageBreak, TdxCharacters.ColumnBreak]);
{$ENDIF}
end;

{ TdxUrlAutoCorrectProvider }

class constructor TdxUrlAutoCorrectProvider.Initialize;
begin
  FUrlRegex := TRegEx.Create('(?:[a-z][\w-]+:(?:/{1,3}([^./]*:[^./]*@){0,1})|www\d{0,3}[.]|ftp[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\([^\s<>]*\))+(?:\([^\s<>]*\)|[^\s`!()\[\]{};:'#$27'".,<>?«»“”‘’])', [roIgnoreCase]);
  FEmailRegex := TRegEx.Create('(mailto:)?[-\w!#$%&'#$27'*+/=?^_`{|}~]+(?:\.[-\w!#$%&'#$27'*+/=?^_`{|}~]+)*@(?:\w+([-\w]*\w)?\.)*[\w]+', [roIgnoreCase]);
end;

function TdxUrlAutoCorrectProvider.IsSeparator(ACh: Char): Boolean;
begin
  Result := IsTriggerChar(ACh);
end;

function TdxUrlAutoCorrectProvider.CalculateWordReplacement(const AInfo: TdxAutoCorrectInfo): TValue;
var
  AResult: TdxHyperlinkInfo;
  AWord: string;
begin
  AWord := AInfo.Text;
  if not IsValidUrl(AWord) then
    Exit(TValue.Empty);

  AResult := TdxHyperlinkInfo.Create;
  AResult.NavigateUri := CreateNavigateUri(AWord);
  Result := TValue.From(AResult);
end;

function TdxUrlAutoCorrectProvider.CreateNavigateUri(const AWord: string): string;
begin
  if not TdxStringHelper.Contains(AWord, ':') and TdxStringHelper.Contains(AWord, '@') then
    Exit('mailto:' + AWord)
  else
    Exit(AWord);
end;

function TdxUrlAutoCorrectProvider.IsValidUrl(const AText: string): Boolean;
var
  AMatch: TMatch;
begin
  AMatch := FEmailRegex.Match(AText);
  if (AMatch.Index = 1) and (AMatch.Length = Length(AText)) then
    Exit(True);

  AMatch := FUrlRegex.Match(AText);
  if (AMatch.Index = 1) and (AMatch.Length = Length(AText)) then
    Exit(True);

  Result := False;
end;

function TdxUrlAutoCorrectProvider.IsAutoCorrectAllowed(const AInfo: TdxAutoCorrectInfo): Boolean;
var
  AField: TdxField;
begin
  AField := DocumentServer.DocumentModel.Selection.PieceTable.FindFieldByRunIndex(AInfo.&End.RunIndex);
  Result := AField = nil;
end;

function TdxUrlAutoCorrectProvider.CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
begin
  if not DocumentModel.AutoCorrectOptions.DetectUrls then
    Exit(TdxAutoCorrectInfo.Null);

  Result := inherited CalculateAutoCorrectInfo;
end;

{ TdxSpellCheckerAutoCorrectProvider }

function TdxSpellCheckerAutoCorrectProvider.CalculateWordReplacement(const AInfo: TdxAutoCorrectInfo): TValue;
var
  ASpellChecker: IdxSpellChecker3;
  ATextController: TdxAutoCorrectRichEditTextController;
  AInnerControl: IdxInnerControl;
  AAutoCorrectInfo: TdxSpellCheckerAutoCorrectWordInfo;
begin
  ASpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
  if (ASpellChecker = nil) or not ASpellChecker.AutoCorrectOptions.Active then
    Exit(TValue.Empty);

  AInnerControl := DocumentServer as IdxInnerControl;
  if AInnerControl = nil then
    Exit(TValue.Empty);

  ATextController := TdxAutoCorrectRichEditTextController.Create(AInnerControl);
  try
    AAutoCorrectInfo := TdxSpellCheckerAutoCorrectWordInfo.Create;
    AAutoCorrectInfo.SpellingAreaFinish := TdxDocumentPosition.Create(TdxDocumentModelPosition.FromDocumentEnd(AInfo.&End.PieceTable));
    AAutoCorrectInfo.SpellingAreaStart := TdxDocumentPosition.Create(TdxDocumentModelPosition.Create(AInfo.&End.PieceTable));
    AAutoCorrectInfo.Word := AInfo.Text;
    AAutoCorrectInfo.WordPositionFinish := TdxDocumentPosition.Create(AInfo.&End);
    AAutoCorrectInfo.WordPositionStart := TdxDocumentPosition.Create(AInfo.Start);
    if ASpellChecker.AutoCorrect(TdxInnerRichEditControl(AInnerControl).Owner.Control,
        ATextController, AAutoCorrectInfo) then
    begin
      FRule := AAutoCorrectInfo.Rule;
      Result := AAutoCorrectInfo.Word;
    end
    else
      Result := TValue.Empty;
  finally
    ATextController.Free;
  end;
end;

function TdxSpellCheckerAutoCorrectProvider.CalculateAutoCorrectInfo: TdxAutoCorrectInfo;
var
  ASpellChecker: IdxSpellChecker3;
begin
  ASpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
  if (ASpellChecker = nil) or not ASpellChecker.AutoCorrectOptions.Active then
    Exit(TdxAutoCorrectInfo.Null);

  Result := inherited CalculateAutoCorrectInfo;
  Result.Rule := FRule;
end;

{ TdxRichEditAutoCorrectHistoryItem }

constructor TdxRichEditAutoCorrectHistoryItem.Create(APieceTable: TdxCustomPieceTable;
  const ARule: IdxSpellCheckerAutoCorrectCustomRule);
begin
  inherited Create(APieceTable);
  FRule := ARule;
end;

procedure TdxRichEditAutoCorrectHistoryItem.RedoCore;
begin
  FSpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
end;

procedure TdxRichEditAutoCorrectHistoryItem.UndoCore;
begin
  if (FSpellChecker = nil) or (FSpellChecker <> TdxSpellCheckerInstance.ISpellChecker3) or (FRule = nil) then
    Exit;
  FRule.Undo;
end;

end.
