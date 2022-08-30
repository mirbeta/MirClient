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

unit dxRichEdit.Dialogs.HyperlinkFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Hyperlink;

type

  { TdxHyperlinkUriType }

  TdxHyperlinkUriType = (
    None,
    Url,
    Anchor
  );

  { TdxHyperlinkFormControllerParameters }

  TdxHyperlinkFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FHyperlinkInfo: TdxHyperlinkInfo;
    FRunInfo: TdxRunInfo;
    FInitialTextToDisplay: string;
    FTextToDisplay: string;
    function GetTextSource: TdxTextToDisplaySource;
  public
    constructor Create(const AControl: IdxRichEditControl; AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo);

    property HyperlinkInfo: TdxHyperlinkInfo read FHyperlinkInfo;
    property RunInfo: TdxRunInfo read FRunInfo;
    property TextToDisplay: string read FTextToDisplay write FTextToDisplay;
    property InitialTextToDisplay: string read FInitialTextToDisplay write FInitialTextToDisplay;
    property TextSource: TdxTextToDisplaySource read GetTextSource;
  end;

  { TdxHyperlinkFormController }

  TdxHyperlinkFormController = class(TdxFormController)
  strict private
    FNavigateUri: string;
    FAnchor: string;
    FToolTip: string;
    FTarget: string;
    FUriType: TdxHyperlinkUriType;
    FControllerParameters: TdxHyperlinkFormControllerParameters;
    FHyperlinkInfo: TdxHyperlinkInfo;
    FControl: IdxRichEditControl;
    FRunInfo: TdxRunInfo;
    function GetTextToDisplay: string; inline;
    function GetTextSource: TdxTextToDisplaySource; inline;
    procedure SetTextToDisplay(const Value: string); inline;
  protected
    procedure InitializeController;
    function CanUseNavigateUri(const ANavigateUri: string; const AAnchor: string): Boolean;
    function BookmarkExists(const AName: string): Boolean;
    procedure SetNavigateUriCore;
    procedure SetAnchorCore;
    procedure ApplyTextToDisplay;
  public
    constructor Create(AControllerParameters: TdxHyperlinkFormControllerParameters);
    procedure ApplyChanges; overload; override;
    procedure ApplyChanges(AUseNavigateUri: Boolean); reintroduce; overload;
    function CanChangeDisplayText: Boolean;
    function GetText(const AStart, AEnd: TdxDocumentModelPosition): string;

    property HyperlinkInfo: TdxHyperlinkInfo read FHyperlinkInfo;
    property TextSource: TdxTextToDisplaySource read GetTextSource;

    property UriType: TdxHyperlinkUriType read FUriType write FUriType;
    property TextToDisplay: string read GetTextToDisplay write SetTextToDisplay;
    property NavigateUri: string read FNavigateUri write FNavigateUri;
    property Anchor: string read FAnchor write FAnchor;
    property ToolTip: string read FToolTip write FToolTip;
    property Target: string read FTarget write FTarget;
    property Control: IdxRichEditControl read FControl;
  end;

implementation

uses
  StrUtils, dxTypeHelpers,
  dxStringHelper,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.Bookmarks;

{ TdxHyperlinkFormControllerParameters }

constructor TdxHyperlinkFormControllerParameters.Create(const AControl: IdxRichEditControl; AHyperlinkInfo: TdxHyperlinkInfo; ARunInfo: TdxRunInfo);
begin
  inherited Create(AControl);
  FHyperlinkInfo := AHyperlinkInfo;
  FRunInfo := ARunInfo;
end;

function TdxHyperlinkFormControllerParameters.GetTextSource: TdxTextToDisplaySource;
begin
  if FTextToDisplay = FInitialTextToDisplay then
    Result := TdxTextToDisplaySource.ExistingText
  else
    Result := TdxTextToDisplaySource.NewText;
end;

{ TdxHyperlinkFormController }

constructor TdxHyperlinkFormController.Create(AControllerParameters: TdxHyperlinkFormControllerParameters);
begin
  inherited Create;
  FControllerParameters := AControllerParameters;
  FControl := AControllerParameters.Control;
  FHyperlinkInfo := AControllerParameters.HyperlinkInfo;
  FRunInfo := AControllerParameters.RunInfo;
  InitializeController;
end;

function TdxHyperlinkFormController.GetTextToDisplay: string;
begin
  Result := FControllerParameters.TextToDisplay;
end;

function TdxHyperlinkFormController.GetTextSource: TdxTextToDisplaySource;
begin
  Result := FControllerParameters.TextSource;
end;

procedure TdxHyperlinkFormController.InitializeController;
begin
  if CanUseNavigateUri(HyperlinkInfo.NavigateUri, HyperlinkInfo.Anchor) then
  begin
    NavigateUri := HyperlinkInfo.CreateUrl;
    if NavigateUri = '' then
      NavigateUri := 'http://';
    UriType := TdxHyperlinkUriType.Url;
  end
  else
  begin
    Anchor := HyperlinkInfo.Anchor;
    UriType := TdxHyperlinkUriType.Anchor;
  end;
  ToolTip := HyperlinkInfo.ToolTip;
  Target := HyperlinkInfo.Target;
  if FRunInfo.NormalizedEnd^ > FRunInfo.NormalizedStart^ then
    if CanChangeDisplayText then
      TextToDisplay := Trim(GetText(FRunInfo.NormalizedStart^, FRunInfo.NormalizedEnd^))
    else
      TextToDisplay := '';

  FControllerParameters.InitialTextToDisplay := TextToDisplay;
end;

function TdxHyperlinkFormController.CanUseNavigateUri(const ANavigateUri: string; const AAnchor: string): Boolean;
begin
  Result := (ANavigateUri <> '') or (AAnchor = '') or (not BookmarkExists(AAnchor));
end;

function TdxHyperlinkFormController.BookmarkExists(const AName: string): Boolean;
var
  ABookmarks: TdxBookmarkList;
  I: Integer;
begin
  ABookmarks := Control.InnerControl.DocumentModel.GetBookmarks;
  try
    for I := 0 to ABookmarks.Count - 1 do
      if AName = ABookmarks[I].Name then
        Exit(True);
    Result := False;
  finally
    ABookmarks.Free;
  end;
end;

procedure TdxHyperlinkFormController.ApplyChanges;
begin
  if UriType = TdxHyperlinkUriType.Anchor then
    SetAnchorCore
  else
    SetNavigateUriCore;
  HyperlinkInfo.ToolTip := ToolTip;
  HyperlinkInfo.Target := Target;
  HyperlinkInfo.Visited := False;
  ApplyTextToDisplay;
end;

procedure TdxHyperlinkFormController.ApplyChanges(AUseNavigateUri: Boolean);
begin
  if AUseNavigateUri then
    SetNavigateUriCore
  else
    SetAnchorCore;
  HyperlinkInfo.ToolTip := ToolTip;
  HyperlinkInfo.Target := Target;
  HyperlinkInfo.Visited := False;
  ApplyTextToDisplay;
end;

function TdxHyperlinkFormController.CanChangeDisplayText: Boolean;
begin
  Result := (FRunInfo.Start.ParagraphIndex = FRunInfo.&End.ParagraphIndex) and
    not FControl.InnerControl.DocumentModel.Selection.PieceTable.HasInlinePicture(FRunInfo);
end;

function TdxHyperlinkFormController.GetText(const AStart, AEnd: TdxDocumentModelPosition): string;
var
  AFilter: TdxVisibleTextFilterBase;
begin
  AFilter := TdxPieceTable(AStart.PieceTable).VisibleTextFilter;
  Result := TdxPieceTable(AStart.PieceTable).GetFilteredPlainText(AStart, AEnd, AFilter.IsRunVisible);
end;

procedure TdxHyperlinkFormController.SetNavigateUriCore;
var
  AUrl, AAnchor, ANavigateUri: string;
  AAnchorStartIndex: Integer;
begin
  if NavigateUri <> '' then
    AUrl := Trim(NavigateUri)
  else
    AUrl := '';
  AAnchorStartIndex := TdxStringHelper.LastIndexOf(AUrl, '#');
  if AAnchorStartIndex > 0 then
    AAnchor := TdxStringHelper.Substring(AUrl, AAnchorStartIndex + 1)
  else
    AAnchor := '';
  HyperlinkInfo.Anchor := AAnchor;
  if AAnchor = '' then
    ANavigateUri := AUrl
  else
    ANavigateUri := TdxStringHelper.Remove(AUrl, AAnchorStartIndex);
  HyperlinkInfo.NavigateUri := TdxHyperlinkUriHelper.EnsureUriIsValid(ANavigateUri);
end;

procedure TdxHyperlinkFormController.SetTextToDisplay(const Value: string);
begin
  FControllerParameters.TextToDisplay := Value;
end;

procedure TdxHyperlinkFormController.SetAnchorCore;
begin
  HyperlinkInfo.NavigateUri := '';
  HyperlinkInfo.Anchor := Anchor;
end;

procedure TdxHyperlinkFormController.ApplyTextToDisplay;
begin
  if (TextToDisplay <> '') or (TextSource = TdxTextToDisplaySource.ExistingText) then
    Exit;
  if NavigateUri <> '' then
    TextToDisplay := NavigateUri
  else
    TextToDisplay := Anchor;
end;


end.
