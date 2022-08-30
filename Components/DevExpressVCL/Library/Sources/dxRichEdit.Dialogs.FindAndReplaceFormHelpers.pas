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

unit dxRichEdit.Dialogs.FindAndReplaceFormHelpers;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Classes, SysUtils, Menus, StdCtrls, cxEdit,
  Character,
  dxRichEdit.NativeApi,
  dxRichEdit.View.Core,
  dxRichEdit.Commands.FindAndReplace,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FindAndReplace;

type
  TdxSearchFormActivePage = (
    Find,
    Replace
  );

  TdxTextSearchDirection = (
    All,
    Down,
    Up
  );

  { TdxSearchCompleteMessageHelper }

  TdxSearchCompleteMessageHelper = class abstract
  public
    class function GetFindInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string; static;
    class function GetFindCompleteMessage(E: TdxSearchCompleteEventArgs): string; static;
    class function CreateFindCompleteMessage(AMatchCount: Integer; const AMessageTemplate: string): string; static;
    class function GetReplaceAllInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string; static;
    class function GetReplaceAllCompleteMessage(E: TdxSearchCompleteEventArgs): string; static;
    class function CreateReplaceAllCompleteMessage(AMatchCount: Integer; const AMessageTemplate: string): string; static;
    class function GetCommonMessageTemplate(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string; static;
    class function GetIntermediateResultMessage(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string; static;
    class function GetContinueSearchQuestion(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string; static;
    class function GetSearchInSelectionCompleteMessage: string; static;
    class function GetSearchInForwardDirectionCompleteMessage: string; static;
    class function GetSearchInBackwardDirectionCompleteMessage: string; static;
    class function GetSearchCompleteMessage: string; static;
    class function GetContinueSearchInRemainderQuestion: string; static;
    class function GetContinueSearchFromBeginningQuestion: string; static;
    class function GetContinueSearchFromEndQuestion: string; static;
    class function GetSearchItemNotFoundMessage: string; static;
    class function GetReplacementsCountMessage(AReplacementsCount: Integer): string; static;
  end;

  { TdxSearchHelperBase }

  TdxSearchHelperBase = class abstract
  strict private
    FControl: IdxRichEditControl;
    FSearchDirection: TdxTextSearchDirection;
    function GetDocumentModel: TdxDocumentModel;
    function GetSearchContext: TdxSearchContext;
    function GetSearchParameters: TdxSearchParameters;
  protected
    procedure OnExceptionsOccurs(E: Exception); virtual;
    function CreateFindCommand: TdxFindNextAndSelectCommandBase; virtual;
    function CreateReplaceCommand: TdxReplaceCommandBase; virtual;
    function CreateReplaceAllCommand: TdxReplaceAllCommandBase; virtual;
    procedure OnStopSearching(const AMessage: string); virtual; abstract;
    function ShouldContinueSearch(const AMessage: string): Boolean; overload; virtual; abstract;
    function ShouldContinueSearch(ASearchScope: TdxSearchScope; const AMessage: string): Boolean; overload;
    procedure OnInvalidRegExp(const AMessage: string); overload; virtual;
    procedure OnInvalidRegExp; overload; virtual;
    procedure OnSearchComplete(E: TdxSearchCompleteEventArgs); overload; virtual;
    procedure OnFindComplete(E: TdxSearchCompleteEventArgs);
    procedure OnReplaceAllComplete(E: TdxSearchCompleteEventArgs);
    function GetFindInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
    function GetFindCompleteMessage(E: TdxSearchCompleteEventArgs): string;
    function GetReplaceAllInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
    function GetReplaceAllCompleteMessage(E: TdxSearchCompleteEventArgs): string;
    function GetIncorrectRegExpMessage: string;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property SearchContext: TdxSearchContext read GetSearchContext;
    property SearchParameters: TdxSearchParameters read GetSearchParameters;
  public
    constructor Create(const AControl: IdxRichEditControl);
    procedure ExecuteFindCommand; virtual;
    procedure ExecuteReplaceCommand; virtual;
    procedure ExecuteReplaceAllCommand; virtual;

    property Direction: TdxTextSearchDirection read FSearchDirection write FSearchDirection;
    property Control: IdxRichEditControl read FControl;
  end;

  { TdxSearchFormControllerParameters }

  TdxSearchFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FActivePage: TdxSearchFormActivePage;
  public
    constructor Create(const AControl: IdxRichEditControl; AActivePage: TdxSearchFormActivePage);
    property ActivePage: TdxSearchFormActivePage read FActivePage write FActivePage;
  end;

  { TdxSearchFormControllerBase }

  TdxSearchFormControllerBase = class abstract(TdxFormController)
  strict private const
    ParagraphStartPattern = '(?:(?<=^])|(?<=[\r\n])())';
    ParagraphEndPattern = '[\r\n]';
  strict private
    FCaseSensitive: Boolean;
    FFindWholeWord: Boolean;
    FRegularExpression: Boolean;
    FSearchString: string;
    FReplaceString: string;
    FControl: IdxRichEditControl;
    FSearchHelper: TdxSearchHelperBase;
    function GetDirection: TdxTextSearchDirection;
    function GetDocumentModel: TdxDocumentModel;
    function GetSearchParameters: TdxSearchParameters;
    function GetSearchContext: TdxSearchContext;
    procedure SetDirection(const Value: TdxTextSearchDirection);
  protected
    function CreateSearchHelper: TdxSearchHelperBase; virtual; abstract;
    function GetSelectedText: string;
    function ShouldSearchInSelection(const ASelectedText: string): Boolean; virtual;
    procedure ValidateSelectedTextOnSearchPossibility; virtual;
    procedure PopulateSearchParameters;
    function GetRegularExpression: string; virtual;
    function IsParagraphExpression(const APattern: string): Boolean;

    property SearchHelper: TdxSearchHelperBase read FSearchHelper;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property SearchParameters: TdxSearchParameters read GetSearchParameters;
    property SearchContext: TdxSearchContext read GetSearchContext;
  public
    constructor Create(AControllerParameters: TdxSearchFormControllerParameters);
    destructor Destroy; override;
    function TryGetSearchStringFromSelection(out AResult: string): Boolean;
    procedure Find;
    procedure Replace;
    procedure ReplaceAll;

    property SearchString: string read FSearchString write FSearchString;
    property ReplaceString: string read FReplaceString write FReplaceString;
    property FindWholeWord: Boolean read FFindWholeWord write FFindWholeWord;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property RegularExpression: Boolean read FRegularExpression write FRegularExpression;
    property Control: IdxRichEditControl read FControl;
    property Direction: TdxTextSearchDirection read GetDirection write SetDirection;
  end;

  { TdxInsertRegexItemCommand }

  TdxInsertRegexItemCommand = class(TdxRichEditCommand)
  private
    FMenuCaption: string;
    FInsertStr: string;
    FEditor: TcxCustomEdit;
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const ARichEditControl: IdxRichEditControl; AEditor: TcxCustomEdit;
      const AMenuCaption, AInsertStr: string; AShowInsertStr: Boolean = True); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;
    procedure UpdateUIState(const AState: IdxCommandUIState); override;
    property MenuCaption: string read FMenuCaption;
    property Editor: TcxCustomEdit read FEditor write FEditor;
  end;

  { TInsertRegexMenuItem }

  TInsertRegexMenuItem = class(TMenuItem)
  private
    FItemCommand: TdxInsertRegexItemCommand;
  public
    constructor Create(AOwner: TComponent; const ARichEditControl: IdxRichEditControl; AEditor: TcxCustomEdit;
      const AMenuCaption, AInsertStr: string; AShowInsertStr: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Click; override;
    property ItemCommand: TdxInsertRegexItemCommand read FItemCommand;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions.Strs,
  Variants, dxCore;

{ TdxSearchCompleteMessageHelper }

class function TdxSearchCompleteMessageHelper.GetFindInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
var
  AMessageTemplate: string;
begin
  AMessageTemplate := GetCommonMessageTemplate(E.SearchScope, E.Direction);
  Result := CreateFindCompleteMessage(E.MatchCount, AMessageTemplate);
end;

class function TdxSearchCompleteMessageHelper.GetFindCompleteMessage(E: TdxSearchCompleteEventArgs): string;
var
  AMessageTemplate: string;
begin
  AMessageTemplate := GetSearchCompleteMessage + ' %s';
  Result := CreateFindCompleteMessage(E.MatchCount, AMessageTemplate);
end;

class function TdxSearchCompleteMessageHelper.CreateFindCompleteMessage(AMatchCount: Integer; const AMessageTemplate: string): string;
var
  AResultMessage: string;
begin
  if AMatchCount > 0 then
    AResultMessage := ''
  else
    AResultMessage := GetSearchItemNotFoundMessage;
  Result := Format(AMessageTemplate, [AResultMessage]);
end;

class function TdxSearchCompleteMessageHelper.GetReplaceAllInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
var
  AMessageTemplate: string;
begin
  AMessageTemplate := GetCommonMessageTemplate(E.SearchScope, E.Direction);
  Result := CreateReplaceAllCompleteMessage(E.MatchCount, AMessageTemplate);
end;

class function TdxSearchCompleteMessageHelper.GetReplaceAllCompleteMessage(E: TdxSearchCompleteEventArgs): string;
var
  AMessageTemplate: string;
begin
  AMessageTemplate := GetSearchCompleteMessage + ' %s';
  Result := CreateReplaceAllCompleteMessage(E.MatchCount, AMessageTemplate);
end;

class function TdxSearchCompleteMessageHelper.CreateReplaceAllCompleteMessage(AMatchCount: Integer; const AMessageTemplate: string): string;
var
  AResultMessage: string;
begin
  if AMatchCount > 0 then
    AResultMessage := GetReplacementsCountMessage(AMatchCount)
  else
    AResultMessage := GetSearchItemNotFoundMessage;
  Result := Format(AMessageTemplate, [AResultMessage]);
end;

class function TdxSearchCompleteMessageHelper.GetCommonMessageTemplate(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string;
var
  ASearchResultMessage, AContinueSearchQuestion: string;
begin
  ASearchResultMessage := GetIntermediateResultMessage(ASearchScope, ADirection);
  AContinueSearchQuestion := GetContinueSearchQuestion(ASearchScope, ADirection);
  Result := ASearchResultMessage + ' %s ' + AContinueSearchQuestion;
end;

class function TdxSearchCompleteMessageHelper.GetIntermediateResultMessage(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string;
begin
  if ASearchScope = TdxSearchScope.SelectedText then
    Result := GetSearchInSelectionCompleteMessage
  else
    if (ASearchScope = TdxSearchScope.BelowSelectedText) and (ADirection = TdxDirection.Forward) then
      Result := GetSearchInForwardDirectionCompleteMessage
    else
      if (ASearchScope = TdxSearchScope.AboveSelectedText) and (ADirection = TdxDirection.Backward) then
        Result := GetSearchInBackwardDirectionCompleteMessage
      else
        Result := '';
end;

class function TdxSearchCompleteMessageHelper.GetContinueSearchQuestion(ASearchScope: TdxSearchScope; ADirection: TdxDirection): string;
begin
  if ASearchScope = TdxSearchScope.SelectedText then
    Result := GetContinueSearchInRemainderQuestion
  else
    if (ASearchScope = TdxSearchScope.BelowSelectedText) and (ADirection = TdxDirection.Forward) then
      Result := GetContinueSearchFromBeginningQuestion
    else
      if (ASearchScope = TdxSearchScope.AboveSelectedText) and (ADirection = TdxDirection.Backward) then
        Result := GetContinueSearchFromEndQuestion
      else
        Result := '';
end;

class function TdxSearchCompleteMessageHelper.GetSearchInSelectionCompleteMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionSearchInSelectionComplete);
end;

class function TdxSearchCompleteMessageHelper.GetSearchInForwardDirectionCompleteMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionSearchInForwardDirectionComplete);
end;

class function TdxSearchCompleteMessageHelper.GetSearchInBackwardDirectionCompleteMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionSearchInBackwardDirectionComplete);
end;

class function TdxSearchCompleteMessageHelper.GetSearchCompleteMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionSearchComplete);
end;

class function TdxSearchCompleteMessageHelper.GetContinueSearchInRemainderQuestion: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionContinueSearchInRemainderQuestion);
end;

class function TdxSearchCompleteMessageHelper.GetContinueSearchFromBeginningQuestion: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionContinueSearchFromBeginningQuestion);
end;

class function TdxSearchCompleteMessageHelper.GetContinueSearchFromEndQuestion: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionContinueSearchFromEndQuestion);
end;

class function TdxSearchCompleteMessageHelper.GetSearchItemNotFoundMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionSearchItemNotFound);
end;

class function TdxSearchCompleteMessageHelper.GetReplacementsCountMessage(AReplacementsCount: Integer): string;
begin
  Result := Format(cxGetResourceString(@sdxRichEditExceptionReplacementsCount), [IntToStr(AReplacementsCount)]);
end;

{ TdxSearchHelperBase }

constructor TdxSearchHelperBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  Assert(Assigned(AControl), 'control');
  FControl := AControl;
end;

function TdxSearchHelperBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.InnerControl.DocumentModel;
end;

function TdxSearchHelperBase.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

function TdxSearchHelperBase.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

procedure TdxSearchHelperBase.ExecuteFindCommand;
var
  ACmd: TdxFindNextAndSelectCommandBase;
begin
  ACmd := CreateFindCommand;
  try
    try
      ACmd.Execute;
    except
      on E: Exception do
        OnExceptionsOccurs(e);
    end;
  finally
    ACmd.Free;
  end;
end;

procedure TdxSearchHelperBase.OnExceptionsOccurs(E: Exception);
begin
  if DocumentModel.SearchParameters.UseRegularExpression then
    OnInvalidRegExp
  else
    raise E;
end;

function TdxSearchHelperBase.CreateFindCommand: TdxFindNextAndSelectCommandBase;
begin
  if Direction = TdxTextSearchDirection.Up then
    Result := TdxFindAndSelectBackwardCommand.Create(Control)
  else
    Result := TdxFindAndSelectForwardCommand.Create(Control);
end;

procedure TdxSearchHelperBase.ExecuteReplaceCommand;
var
  ACmd: TdxReplaceCommandBase;
begin
  ACmd := CreateReplaceCommand;
  try
    try
      ACmd.Execute;
    except
      on E: Exception do
        OnExceptionsOccurs(e);
    end;
  finally
    ACmd.Free;
  end;
end;

function TdxSearchHelperBase.CreateReplaceCommand: TdxReplaceCommandBase;
begin
  if Direction = TdxTextSearchDirection.Up then
    Result := TdxReplaceBackwardCommand.Create(Control)
  else
    Result := TdxReplaceForwardCommand.Create(Control);
end;

procedure TdxSearchHelperBase.ExecuteReplaceAllCommand;
var
  ACmd: TdxReplaceAllCommandBase;
begin
  ACmd := CreateReplaceAllCommand;
  try
    try
      ACmd.Execute;
    except
      on E: Exception do
        OnExceptionsOccurs(e);
    end;
  finally
    ACmd.Free;
  end;
end;

function TdxSearchHelperBase.CreateReplaceAllCommand: TdxReplaceAllCommandBase;
begin
  if Direction = TdxTextSearchDirection.Up then
    Result := TdxReplaceAllBackwardCommand.Create(Control)
  else
    Result := TdxReplaceAllForwardCommand.Create(Control);
end;

procedure TdxSearchHelperBase.OnInvalidRegExp(const AMessage: string);
begin
end;

procedure TdxSearchHelperBase.OnInvalidRegExp;
begin
  OnInvalidRegExp(GetIncorrectRegExpMessage);
end;

procedure TdxSearchHelperBase.OnSearchComplete(E: TdxSearchCompleteEventArgs);
begin
  if E.Cancel then
    Exit;

  if E.Action = TdxSearchAction.ReplaceAll then
    OnReplaceAllComplete(E)
  else
    OnFindComplete(E);
end;

procedure TdxSearchHelperBase.OnFindComplete(E: TdxSearchCompleteEventArgs);
var
  AMessage: string;
begin
  AMessage := GetFindInSingleDirectionCompleteMessage(E);
  if not E.EntireDocument and ShouldContinueSearch(E.SearchScope, AMessage) then
    E.Continue := True
  else
  begin
    OnStopSearching(GetFindCompleteMessage(E));
    SearchContext.StopSearching;
  end;
end;

procedure TdxSearchHelperBase.OnReplaceAllComplete(E: TdxSearchCompleteEventArgs);
var
  AMessage: string;
begin
  AMessage := GetReplaceAllInSingleDirectionCompleteMessage(E);
  if not E.EntireDocument and ShouldContinueSearch(E.SearchScope, AMessage) then
    E.Continue := True
  else
  begin
    OnStopSearching(GetReplaceAllCompleteMessage(E));
    SearchContext.StopSearching;
  end;
end;

function TdxSearchHelperBase.ShouldContinueSearch(ASearchScope: TdxSearchScope; const AMessage: string): Boolean;
begin
  if (Direction = TdxTextSearchDirection.All) and (ASearchScope <> TdxSearchScope.AboveSelectedText) then
    Result := True
  else
    Result := ShouldContinueSearch(AMessage);
end;

function TdxSearchHelperBase.GetFindInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
begin
  Result := TdxSearchCompleteMessageHelper.GetFindInSingleDirectionCompleteMessage(E);
end;

function TdxSearchHelperBase.GetFindCompleteMessage(E: TdxSearchCompleteEventArgs): string;
begin
  Result := TdxSearchCompleteMessageHelper.GetFindCompleteMessage(E);
end;

function TdxSearchHelperBase.GetReplaceAllInSingleDirectionCompleteMessage(E: TdxSearchCompleteEventArgs): string;
begin
  Result := TdxSearchCompleteMessageHelper.GetReplaceAllInSingleDirectionCompleteMessage(E);
end;

function TdxSearchHelperBase.GetReplaceAllCompleteMessage(E: TdxSearchCompleteEventArgs): string;
begin
  Result := TdxSearchCompleteMessageHelper.GetReplaceAllCompleteMessage(E);
end;

function TdxSearchHelperBase.GetIncorrectRegExpMessage: string;
begin
  Result := cxGetResourceString(@sdxRichEditExceptionIncorrectPattern);
end;

{ TdxSearchFormControllerParameters }

constructor TdxSearchFormControllerParameters.Create(const AControl: IdxRichEditControl; AActivePage: TdxSearchFormActivePage);
begin
  inherited Create(AControl);
  FActivePage := AActivePage;
end;

{ TdxSearchFormControllerBase }

constructor TdxSearchFormControllerBase.Create(AControllerParameters: TdxSearchFormControllerParameters);
begin
  inherited Create;
  Assert(Assigned(AControllerParameters), 'controllerParameters');
  FControl := AControllerParameters.Control;

  FSearchHelper := CreateSearchHelper;
end;

function TdxSearchFormControllerBase.GetDirection: TdxTextSearchDirection;
begin
  Result := SearchHelper.Direction;
end;

function TdxSearchFormControllerBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.InnerControl.DocumentModel;
end;

function TdxSearchFormControllerBase.GetSearchParameters: TdxSearchParameters;
begin
  Result := DocumentModel.SearchParameters;
end;

function TdxSearchFormControllerBase.GetSearchContext: TdxSearchContext;
begin
  Result := DocumentModel.SearchContext;
end;

function TdxSearchFormControllerBase.TryGetSearchStringFromSelection(out AResult: string): Boolean;
var
  ASelectedText: string;
begin
  AResult := '';
  if not DocumentModel.IsTextSelectedOnly then
    Exit(False);

  ASelectedText := GetSelectedText;
  if ASelectedText = '' then
    Exit(False);

  if not ShouldSearchInSelection(ASelectedText) then
  begin
    AResult := ASelectedText;
    Result := True;
  end
  else
    Result := False;
end;

function TdxSearchFormControllerBase.GetSelectedText: string;
begin
  Result := Trim(DocumentModel.GetSelectionText);
end;

procedure TdxSearchFormControllerBase.SetDirection(const Value: TdxTextSearchDirection);
begin
  SearchHelper.Direction := Value;
end;

function TdxSearchFormControllerBase.ShouldSearchInSelection(const ASelectedText: string): Boolean;
var
  AChar: Char;
begin
  for AChar in ASelectedText do
{$IFDEF DELPHIXE4}
    if AChar.IsWhiteSpace then
{$ELSE}
    if TCharacter.IsWhiteSpace(AChar) then
{$ENDIF}
      Exit(True);
  Result := False;
end;

procedure TdxSearchFormControllerBase.ValidateSelectedTextOnSearchPossibility;
begin
  if not SearchContext.StartOfSearch then
    Exit;

  if ShouldSearchInSelection(GetSelectedText) then
    SearchParameters.FindInSelection := True
  else
    SearchParameters.FindInSelection := False;
end;

procedure TdxSearchFormControllerBase.Find;
begin
  ValidateSelectedTextOnSearchPossibility;
  PopulateSearchParameters;
  SearchHelper.ExecuteFindCommand;
end;

procedure TdxSearchFormControllerBase.Replace;
begin
  ValidateSelectedTextOnSearchPossibility;
  PopulateSearchParameters;
  SearchHelper.ExecuteReplaceCommand;
end;

procedure TdxSearchFormControllerBase.ReplaceAll;
begin
  ValidateSelectedTextOnSearchPossibility;
  PopulateSearchParameters;
  SearchHelper.ExecuteReplaceAllCommand;
end;

procedure TdxSearchFormControllerBase.PopulateSearchParameters;
begin
  if RegularExpression then
    SearchParameters.SearchString := GetRegularExpression
  else
    SearchParameters.SearchString := SearchString;
  SearchParameters.UseRegularExpression := RegularExpression;
  SearchParameters.CaseSensitive := CaseSensitive;
  SearchParameters.FindWholeWord := FindWholeWord;
  SearchParameters.ReplaceString := ReplaceString;
end;

function TdxSearchFormControllerBase.GetRegularExpression: string;
var
  ACount, I: Integer;
  ABuilder: TStringBuilder;
begin
  if IsParagraphExpression(SearchString) then
    Exit(ParagraphStartPattern + #13);
  ACount := Length(SearchString);
  ABuilder := TStringBuilder.Create;
  try
    for I := 1 to ACount do
    begin
      if SearchString[I] = '$' then
        ABuilder.Append(ParagraphEndPattern)
      else
        if (SearchString[I] = '^') and ((I = 1) or (SearchString[I - 1] <> '[')) then
          ABuilder.Append(ParagraphStartPattern)
        else
          ABuilder.Append(SearchString[I]);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxSearchFormControllerBase.IsParagraphExpression(const APattern: string): Boolean;
begin
  Result := APattern = '^$';
end;

destructor TdxSearchFormControllerBase.Destroy;
begin
  FSearchHelper.Free;
  inherited Destroy;
end;

{ TdxInsertRegexItemCommand }

constructor TdxInsertRegexItemCommand.Create(const ARichEditControl: IdxRichEditControl; AEditor: TcxCustomEdit;
  const AMenuCaption, AInsertStr: string; AShowInsertStr: Boolean);
begin
  inherited Create(ARichEditControl);
  FEditor := AEditor;
  if AShowInsertStr then
    FMenuCaption := Format('%s'#9'%s', [AMenuCaption,  AInsertStr])
  else
    FMenuCaption := AMenuCaption;
  FInsertStr := AInsertStr;
end;

class function TdxInsertRegexItemCommand.GetDescription: string;
begin
  Result := '';
end;

class function TdxInsertRegexItemCommand.GetMenuCaption: string;
begin
  Result := '';
end;

procedure TdxInsertRegexItemCommand.ForceExecute(const AState: IdxCommandUIState);
var
  S: string;
begin
  S := VarToStr(FEditor.EditValue) + FInsertStr;
  FEditor.EditValue := S;
  (FEditor.InnerControl as TCustomEdit).SelStart := Length(S);
end;

procedure TdxInsertRegexItemCommand.UpdateUIState(const AState: IdxCommandUIState);
begin
end;

procedure TdxInsertRegexItemCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

{ TInsertRegexMenuItem }

procedure TInsertRegexMenuItem.Click;
begin
  inherited Click;
  if Enabled then
    FItemCommand.Execute;
end;

constructor TInsertRegexMenuItem.Create(AOwner: TComponent; const ARichEditControl: IdxRichEditControl; AEditor: TcxCustomEdit;
  const AMenuCaption, AInsertStr: string; AShowInsertStr: Boolean);
begin
  inherited Create(AOwner);
  FItemCommand := TdxInsertRegexItemCommand.Create(ARichEditControl, AEditor, AMenuCaption, AInsertStr, AShowInsertStr);
  Caption := FItemCommand.MenuCaption;
end;

destructor TInsertRegexMenuItem.Destroy;
begin
  FItemCommand.Free;
  inherited;
end;

end.
