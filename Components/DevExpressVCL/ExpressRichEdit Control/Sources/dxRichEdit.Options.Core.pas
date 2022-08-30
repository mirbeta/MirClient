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

unit dxRichEdit.Options.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types;

type
  TdxRichEditOptionsAction = (
    AllCaps,
    AllowAlternateStyleNames,
    AllowCopy,
    AllowCreateNew,
    AllowCut,
    AllowDrag,
    AllowDrop,
    AllowNameResolution,
    AllowNonLinkedListDefinitions,
    AllowOfficeScrolling,
    AllowOpen,
    AllowPaste,
    AllowPrinting,
    AllowSave,
    AllowSaveAs,
    AllowShowPopupMenu,
    AllowTablesToExtendIntoMargins,
    AllowTouch,
    AllowZooming,
    AlternateImageFolder,
    AsyncImageLoading,
    AuthenticationEMail,
    AuthenticationGroup,
    AuthenticationPassword,
    AuthenticationUserName,
    AutoDetectDocumentCulture,
    AutoDetectEncoding,
    BackColor,
    BackColorExportMode,
    BookmarkColor,
    Bookmarks,
    BookmarkVisibility,
    Bulleted,
    CharacterFormatting,
    CharacterStyle,
    CssPropertiesExportType,
    CurrentFileName,
    CurrentFormat,
    DataSource,
    Date,
    DefaultCharacterPropertiesExportToCss,
    DefaultFileName,
    DefaultFormat,
    DefaultTableCellMargin,
    DefaultTableCellSpacing,
    DetectUrls,
    DuplicateObjectAsMetafile,
    EmbedImages,
    EnablePageBackgroundOnPrint,
    Encoding,
    EndNoteNamePrefix,
    EndNoteNumberStringFormat,
    EndNotes,
    EndNoteSeparator,
    ExportBulletsAndNumbering,
    ExportFinalParagraphMark,
    ExportHiddenText,
    ExportImageSize,
    ExportRootTag,
    ExportToClipboard,
    FallbackFormat,
    FieldCodeEndMarker,
    FieldCodeStartMarker,
    FieldResultEndMarker,
    Fields,
    FloatingObjects,
    FontBold,
    FontItalic,
    FontName,
    FontSize,
    FontSource,
    FontStrikeout,
    FontUnderline,
    FontUnit,
    FootNoteNamePrefix,
    FootNoteNumberStringFormat,
    FootNotes,
    FootNoteSeparator,
    ForeColor,
    ForeColorSource,
    GridLines,
    HeadersFooters,
    Hidden,
    HiddenText,
    HighlightColor,
    HighlightMode,
    HtmlNumberingListExportFormat,
    Hyperlinks,
    IgnoreDeletedText,
    IgnoreFloatProperty,
    IgnoreInsertedText,
    IgnoreMetaCharset,
    IgnoreNoProof,
    IgnoreParagraphOutlineLevel,
    IgnoreParseErrors,
    InlinePictures,
    InsertOptions,
    KeepBookmarksForRemovedRanges,
    KeepExternalImageSize,
    KeepLastParagraph,
    KeepPermissionsForRemovedRanges,
    LimitBookmarkNameTo40Chars,
    LimitFontNameTo31Chars,
    LimitStyleNameTo253Chars,
    ListExportFormat,
    MaintainDocumentSectionSettings,
    MatchHorizontalTableIndentsToTextEdge,
    MaxZoomFactor,
    MergeParagraphsContent,
    MergeUseFirstParagraphStyle,
    MinZoomFactor,
    ModifierKeys,
    MultiLevel,
    OverrideImageResolution,
    OvertypeAllowed,
    PageBreakInsertMode,
    ParagraphFormatting,
    ParagraphFrames,
    ParagraphMark,
    Paragraphs,
    ParagraphStyle,
    ParagraphTabs,
    PasteLineBreakSubstitution,
    PasteSingleCellAsText,
    RangePermissionsBracketsColor,
    RangePermissionsColor,
    RangePermissionsHighlightBracketsColor,
    RangePermissionsHighlightColor,
    RangePermissionsVisibility,
    RegExResultMaxGuaranteedLength,
    ReplaceSpaceWithNonBreakingSpaceInsidePre,
    Script,
    Sections,
    Separator,
    ShowHiddenText,
    ShowLeftIndent,
    ShowRightIndent,
    ShowTabs,
    ShowToolTip,
    Simple,
    Space,
    StrikeoutColor,
    StrikeoutWordsOnly,
    TabCharacter,
    TableCellStyle,
    Tables,
    TableStyle,
    TabMarker,
    TabSymbol,
    ThrowExceptionOnInvalidFormatSwitch,
    Time,
    UnderlineColor,
    UnderlineTocHyperlinks,
    UnderlineWordsOnly,
    Undo,
    UpdateDocVariablesBeforeCopy,
    UpdateDocVariablesBeforePrint,
    UpdateFieldsOnPaste,
    UriExportType,
    UseCurrentCultureForDateTimeFormatting,
    UseFontSubstitution,
    UseHtml5,
    ViewMergedData,
    Visibility,
    WrapContentInGroup);

  TdxRichEditOptionsActions = set of TdxRichEditOptionsAction;

  { TdxRichEditNotificationOptionsChangedArgs }

  TdxRichEditNotificationOptionsChangedArgs = class(TdxOptionChangedEventArgs)
  public type
    TActions = TdxRichEditOptionsActions;
  private
    FActions: TActions;
  public
    constructor Create(const Actions: TActions);
    property Actions: TActions read FActions;
  end;
  TdxRichEditNotificationOptionsChangedEvent = procedure(Sender: TObject; Args: TdxRichEditNotificationOptionsChangedArgs) of object;
  TdxRichEditNotificationOptionsChangedEventHandler = TdxMulticastMethod<TdxRichEditNotificationOptionsChangedEvent>;

  { TdxRichEditNotificationOptions }

  TdxRichEditNotificationOptions = class(TInterfacedPersistent)
  private
    FChanges: TdxRichEditNotificationOptionsChangedArgs.TActions;
    FLockCount: Integer;
    FOnChanged: TdxRichEditNotificationOptionsChangedEventHandler;
  protected
    procedure CreateInnerOptions; virtual;
    procedure DoInnerOptionsChanged(Sender: TObject; E: TdxRichEditNotificationOptionsChangedArgs); virtual;
    procedure SubscribeInnerOptions; virtual;

    procedure DoChanged(Action: TdxRichEditNotificationOptionsChangedArgs.TActions); overload; virtual;
    procedure DoChanged(Action: TdxRichEditOptionsAction); overload; virtual;
    procedure DoReset; virtual;
    function IsLocked: Boolean; virtual;
    procedure Initialize; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    procedure Reset;

    property Changed: TdxRichEditNotificationOptionsChangedEventHandler read FOnChanged;
  end;

  { TdxCustomDocumentCapabilitiesOptions }

  TdxCustomDocumentCapabilitiesOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      Paragraphs          = TdxRichEditOptionsAction.Paragraphs;
      Undo                = TdxRichEditOptionsAction.Undo;
    end;
  strict private
    FParagraphs: TdxDocumentCapability;
    FUndo: TdxDocumentCapability;
    procedure SetParagraphs(const Value: TdxDocumentCapability);
    procedure SetUndo(const Value: TdxDocumentCapability);
  protected
    procedure DoReset; override;
    function IsAllowed(const Value: TdxDocumentCapability): Boolean;
  public
    procedure Assign(Source: TPersistent); override;

    function BookmarksAllowed: Boolean; virtual;
    function CharacterFormattingAllowed: Boolean; virtual;
    function CharacterStyleAllowed: Boolean; virtual;
    function EndNotesAllowed: Boolean; virtual;
    function FieldsAllowed: Boolean; virtual;
    function FloatingObjectsAllowed: Boolean; virtual;
    function FootNotesAllowed: Boolean; virtual;
    function HeadersFootersAllowed: Boolean; virtual;
    function HyperlinksAllowed: Boolean; virtual;
    function InlinePicturesAllowed: Boolean; virtual;
    function ParagraphFormattingAllowed: Boolean; virtual;
    function ParagraphsAllowed: Boolean; virtual;
    function ParagraphFramesAllowed: Boolean; virtual;
    function ParagraphStyleAllowed: Boolean; virtual;
    function ParagraphTabsAllowed: Boolean; virtual;
    function SectionsAllowed: Boolean; virtual;
    function TableCellStyleAllowed: Boolean; virtual;
    function TablesAllowed: Boolean; virtual;
    function TableStyleAllowed: Boolean; virtual;
    function TabSymbolAllowed: Boolean; virtual;
    function UndoAllowed: Boolean; virtual;
  published
    property Paragraphs: TdxDocumentCapability read FParagraphs write SetParagraphs default TdxDocumentCapability.Default;
    property Undo: TdxDocumentCapability read FUndo write SetUndo default TdxDocumentCapability.Default;
  end;

implementation

{ TdxRichEditNotificationOptionsChangedArgs }

constructor TdxRichEditNotificationOptionsChangedArgs.Create(const Actions: TActions);
begin
  inherited Create;
  FActions := Actions;
end;

{ TdxRichEditNotificationOptions }

constructor TdxRichEditNotificationOptions.Create;
begin
  inherited Create;
  Initialize;
  DoReset;
end;

procedure TdxRichEditNotificationOptions.Assign(Source: TPersistent);
begin
  if Source = nil then
    Reset
  else
    if Source is TdxRichEditNotificationOptions then
    begin
    end;
end;

procedure TdxRichEditNotificationOptions.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxRichEditNotificationOptions.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxRichEditNotificationOptions.EndUpdate;
begin
  Dec(FLockCount);
  DoChanged([]);
end;

procedure TdxRichEditNotificationOptions.Reset;
begin
  BeginUpdate;
  try
    DoReset;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditNotificationOptions.CreateInnerOptions;
begin
//do nothing
end;

procedure TdxRichEditNotificationOptions.DoInnerOptionsChanged(Sender: TObject; E: TdxRichEditNotificationOptionsChangedArgs);
begin
  DoChanged(E.Actions);
end;

procedure TdxRichEditNotificationOptions.SubscribeInnerOptions;
begin
//do nothing
end;

procedure TdxRichEditNotificationOptions.DoChanged(Action: TdxRichEditNotificationOptionsChangedArgs.TActions);
var
  Args: TdxRichEditNotificationOptionsChangedArgs;
begin
  FChanges := FChanges + Action;
  if not IsLocked then
  begin
    if FChanges <> [] then
    begin
      if not FOnChanged.Empty then
      begin
        Args := TdxRichEditNotificationOptionsChangedArgs.Create(FChanges);
        try
          FOnChanged.Invoke(Self, Args);
        finally
          Args.Free;
        end;
      end;
    end;
    FChanges := [];
  end;
end;

procedure TdxRichEditNotificationOptions.DoChanged(Action: TdxRichEditOptionsAction);
begin
  DoChanged([Action]);
end;

procedure TdxRichEditNotificationOptions.DoReset;
begin
end;

procedure TdxRichEditNotificationOptions.Initialize;
begin
  CreateInnerOptions;
  SubscribeInnerOptions;
end;

function TdxRichEditNotificationOptions.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

{ TdxCustomDocumentCapabilitiesOptions }

procedure TdxCustomDocumentCapabilitiesOptions.Assign(Source: TPersistent);
var
  ASource: TdxCustomDocumentCapabilitiesOptions;
begin
  BeginUpdate;
  try
    if Source is TdxCustomDocumentCapabilitiesOptions then
    begin
      ASource := TdxCustomDocumentCapabilitiesOptions(Source);
      Paragraphs := ASource.Paragraphs;
      Undo := ASource.Undo;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomDocumentCapabilitiesOptions.DoReset;
begin
  Paragraphs := TdxDocumentCapability.Default;
  Undo := TdxDocumentCapability.Default;
end;

function TdxCustomDocumentCapabilitiesOptions.IsAllowed(const Value: TdxDocumentCapability): Boolean;
begin
  Result := Value in [TdxDocumentCapability.Default, TdxDocumentCapability.Enabled];
end;

function TdxCustomDocumentCapabilitiesOptions.BookmarksAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.CharacterFormattingAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.CharacterStyleAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.EndNotesAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.FieldsAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.FloatingObjectsAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.FootNotesAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.HeadersFootersAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.HyperlinksAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.InlinePicturesAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.ParagraphFormattingAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.ParagraphsAllowed: Boolean;
begin
  Result := IsAllowed(Paragraphs);
end;

function TdxCustomDocumentCapabilitiesOptions.ParagraphFramesAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.ParagraphStyleAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.ParagraphTabsAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.SectionsAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.TableCellStyleAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.TablesAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.TableStyleAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.TabSymbolAllowed: Boolean;
begin
  Result := False;
end;

function TdxCustomDocumentCapabilitiesOptions.UndoAllowed: Boolean;
begin
  Result := IsAllowed(Undo);
end;

procedure TdxCustomDocumentCapabilitiesOptions.SetParagraphs(const Value: TdxDocumentCapability);
begin
  if FParagraphs <> Value then
  begin
    FParagraphs := Value;
    DoChanged(TAction.Paragraphs);
  end;
end;

procedure TdxCustomDocumentCapabilitiesOptions.SetUndo(const Value: TdxDocumentCapability);
begin
  if FUndo <> Value then
  begin
    FUndo := Value;
    DoChanged(TAction.Undo);
  end;
end;

end.
