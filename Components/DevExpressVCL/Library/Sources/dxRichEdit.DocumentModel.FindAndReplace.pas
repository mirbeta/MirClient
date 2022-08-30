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

unit dxRichEdit.DocumentModel.FindAndReplace;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types;

type
  { TdxSearchAction }

  TdxSearchAction = (
    Find,
    Replace,
    ReplaceAll
  );

  { TdxDirection }

  TdxDirection = TdxRichEditSearchDirection;

  { TdxSearchLogicalAction }

  TdxSearchLogicalAction = (
    None,
    FindReplaceForward,
    FindReplaceBackward,
    ReplaceAllForward,
    ReplaceAllBackward
  );

  { TdxSearchScope }

  TdxSearchScope = (
    None,
    SelectedText,
    AboveSelectedText,
    BelowSelectedText
  );

  { TdxSearchState }

  TdxSearchState = (
    FindStart,
    FindInSelection,
    FindAfterSelection,
    FindBeforeSelection,
    FindAfterCaret,
    FindBeforeCaret,
    FindFinish
  );

  { TdxSearchOption }

  TdxSearchOption = TdxRichEditSearchOption;

  TdxSearchOptions = set of TdxSearchOption;

  { TdxSearchContextInfo }

  TdxSearchContextInfo = class
  strict private
    FMatchCount: Integer;
    FReplaceCount: Integer;
    FStartOfIntervalSearch: Boolean;
    FLastResult: TdxDocumentLogPosition;
    FSearchState: TdxSearchState;
    FSearchScope: TdxSearchScope;
  public
    constructor Create;

    property MatchCount: Integer read FMatchCount write FMatchCount;
    property ReplaceCount: Integer read FReplaceCount write FReplaceCount;
    property SearchState: TdxSearchState read FSearchState write FSearchState;
    property SearchScope: TdxSearchScope read FSearchScope write FSearchScope;
    property LastResult: TdxDocumentLogPosition read FLastResult write FLastResult;
    property StartOfIntervalSearch: Boolean read FStartOfIntervalSearch write FStartOfIntervalSearch;
  end;

  { TdxSearchCompleteEventArgs }

  TdxSearchCompleteEventArgs = class(TdxCancelEventArgs)
  strict private
    FAction: TdxSearchAction;
    FDirection: TdxDirection;
    FSearchScope: TdxSearchScope;
    FMatchCount: Integer;
    FReplaceCount: Integer;
    FSearchString: string;
    FReplaceString: string;
    FEntireDocument: Boolean;
    FContinue: Boolean;
  public
    constructor Create(AAction: TdxSearchAction; ADirection: TdxDirection; ASearchScope: TdxSearchScope);
    procedure SetMatchCount(AValue: Integer);
    procedure SetReplaceCount(AValue: Integer);
    procedure SetSearchString(const AValue: string);
    procedure SetReplaceString(const AValue: string);
    procedure SetEntireDocument(AValue: Boolean);

    property Action: TdxSearchAction read FAction;
    property Direction: TdxDirection read FDirection;
    property SearchScope: TdxSearchScope read FSearchScope;
    property MatchCount: Integer read FMatchCount;
    property ReplaceCount: Integer read FReplaceCount;
    property SearchString: string read FSearchString;
    property ReplaceString: string read FReplaceString;
    property EntireDocument: Boolean read FEntireDocument;
    property Continue: Boolean read FContinue write FContinue;
  end;

  { TdxSearchParameters }

  TdxSearchParameters = class
  strict private
    FSearchOptions: TdxSearchOptions;
    FFindInSelection: Boolean;
    FUseRegularExpression: Boolean;
    FSearchString: string;
    FReplaceString: string;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
    function GetFindWholeWord: Boolean;
    procedure SetFindWholeWord(const AValue: Boolean);
    function GetOption(AOption: TdxSearchOption): Boolean;
    procedure SetOption(AOption: TdxSearchOption; AValue: Boolean);
  public

    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property FindWholeWord: Boolean read GetFindWholeWord write SetFindWholeWord;
    property UseRegularExpression: Boolean read FUseRegularExpression write FUseRegularExpression;
    property FindInSelection: Boolean read FFindInSelection write FFindInSelection;
    property SearchString: string read FSearchString write FSearchString;
    property ReplaceString: string read FReplaceString write FReplaceString;
    property SearchOptions: TdxSearchOptions read FSearchOptions;
  end;

  TdxSearchCompleteEvent = procedure (Sender: TObject; E: TdxSearchCompleteEventArgs) of object;

implementation

{ TdxSearchContextInfo }

constructor TdxSearchContextInfo.Create;
begin
  inherited Create;
  FStartOfIntervalSearch := True;
end;

{ TdxSearchCompleteEventArgs }

constructor TdxSearchCompleteEventArgs.Create(AAction: TdxSearchAction; ADirection: TdxDirection; ASearchScope: TdxSearchScope);
begin
  inherited Create;
  FAction := AAction;
  FDirection := ADirection;
  FSearchScope := ASearchScope;
end;

procedure TdxSearchCompleteEventArgs.SetMatchCount(AValue: Integer);
begin
  FMatchCount := AValue;
end;

procedure TdxSearchCompleteEventArgs.SetReplaceCount(AValue: Integer);
begin
  FReplaceCount := AValue;
end;

procedure TdxSearchCompleteEventArgs.SetSearchString(const AValue: string);
begin
  FSearchString := AValue;
end;

procedure TdxSearchCompleteEventArgs.SetReplaceString(const AValue: string);
begin
  FReplaceString := AValue;
end;

procedure TdxSearchCompleteEventArgs.SetEntireDocument(AValue: Boolean);
begin
  FEntireDocument := AValue;
end;

{ TdxSearchParameters }

function TdxSearchParameters.GetCaseSensitive: Boolean;
begin
  Result := GetOption(TdxSearchOption.CaseSensitive);
end;

function TdxSearchParameters.GetFindWholeWord: Boolean;
begin
  Result := GetOption(TdxSearchOption.WholeWord);
end;

function TdxSearchParameters.GetOption(AOption: TdxSearchOption): Boolean;
begin
  Result := AOption in FSearchOptions;
end;

procedure TdxSearchParameters.SetCaseSensitive(const AValue: Boolean);
begin
  SetOption(TdxSearchOption.CaseSensitive, AValue);
end;

procedure TdxSearchParameters.SetFindWholeWord(const AValue: Boolean);
begin
  SetOption(TdxSearchOption.WholeWord, AValue);
end;

procedure TdxSearchParameters.SetOption(AOption: TdxSearchOption; AValue: Boolean);
begin
  if AValue then
    Include(FSearchOptions, AOption)
  else
    Exclude(FSearchOptions, AOption)
end;

end.
