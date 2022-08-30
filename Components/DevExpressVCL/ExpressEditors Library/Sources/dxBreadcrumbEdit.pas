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

unit dxBreadcrumbEdit;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Messages, Graphics, Forms, Controls, ImgList, Menus,
  dxCore, dxCoreClasses, dxMessages, cxClasses, dxCustomTree, dxFading, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxEdit, cxContainer, cxListBox,
  cxDropDownEdit, cxTextEdit, dxAutoCompleteWindow, cxGeometry;

const
  nvipNone = -1;
  nvipControl = 0;
  nvipDelimiter = 1;

  dxBreadcrumbEditDefaultAnimationRestartDelay = 500;
  dxBreadcrumbEditDefaultAnimationSpeed = 10;
  dxBreadcrumbEditDefaultAutoCompleteDropDownRows = 16;
  dxBreadcrumbEditDefaultDropDownRows = 18;
  dxBreadcrumbEditDefaultDropDownIndentValue = 19;
  dxBreadcrumbEditDefaultHeight = 20;
  dxBreadcrumbEditDefaultRecentPathsDropDownRows = 14;
  dxBreadcrumbEditDefaultRecentPathsMaxCount = 0;

  dxBreadcrumbEditStreamVersion = 1;

type
  TdxBreadcrumbEditButton = class;
  TdxBreadcrumbEditController = class;
  TdxBreadcrumbEditDropDownInnerListBox = class;
  TdxCustomBreadcrumbEditProperties = class;
  TdxBreadcrumbEditHitTestInfo = class;
  TdxBreadcrumbEditNode = class;
  TdxBreadcrumbEditPartCustomViewInfo = class;
  TdxBreadcrumbEditPathEditor = class;
  TdxBreadcrumbEditViewInfo = class;

  TdxBreadcrumbEditChange = (bcecAnimation, bcecButtons, bcecLayout, bcecNodes, bcecPathEditor, bcecProgress, bcecSelection);
  TdxBreadcrumbEditChanges = set of TdxBreadcrumbEditChange;
  TdxBreadcrumbEditChangeEvent = procedure (Sender: TObject; AChanges: TdxBreadcrumbEditChanges) of object;

  TdxBreadcrumbEditDropDownIndent = (ddiNone, ddiExplorerLike);

  { IdxBreadcrumbEdit }

  IdxBreadcrumbEdit = interface(IcxLookAndFeelContainer)
  ['{1F299DAA-9C6C-4EBA-B1C3-14E2DF2DEEEE}']
    function GetContainer: TWinControl;
    function GetController: TdxBreadcrumbEditController;
    function GetFont: TFont;
    function GetIsEnabled: Boolean;
    function GetIsFocused: Boolean;
  {$IFNDEF VCLGLASSPAINT}
    function GetOnGlass: Boolean;
  {$ENDIF}
    function GetPathDelimiter: Char;
    function GetProperties: TdxCustomBreadcrumbEditProperties;
    function GetRoot: TdxBreadcrumbEditNode;
    function GetScaleFactor: TdxScaleFactor;
    function GetShowHint: Boolean;
    function UseRightToLeftAlignment: Boolean;
    procedure AdjustAutoSize;
  end;

  { IdxBreadcrumbEditEvents }

  IdxBreadcrumbEditEvents = interface
  ['{58BB7C9A-21D2-4E8C-B95C-E20BC3768E28}']
    procedure NodeDropDownPopup(ANode: TdxBreadcrumbEditNode);
    procedure PathEntered(var ANewPath: string; var AHandled: Boolean);
    procedure PathValidate(const APath: string; var ANode: TdxBreadcrumbEditNode;
      var AErrorText: string; var AError: Boolean);
    procedure PopulateAutoCompleteSuggestions(const APath: string; ASuggestions: TStringList);
    procedure ProcessButtonClick(AButton: TdxBreadcrumbEditButton);
    procedure SelectionChanged;
  end;

  EdxBreadcrumbEditValidationError = class(EdxException);

  { TdxBreadcrumbEditNode }

  TdxBreadcrumbEditNode = class(TdxTreeCustomNode)
  private
    FIsHidden: Boolean;
    function GetFirst: TdxBreadcrumbEditNode;
    function GetItem(AIndex: Integer): TdxBreadcrumbEditNode;
    function GetLast: TdxBreadcrumbEditNode;
    function GetNext: TdxBreadcrumbEditNode;
    function GetParent: TdxBreadcrumbEditNode;
    function GetPathDelimiter: Char;
    function GetPrev: TdxBreadcrumbEditNode;
    function GetRoot: TdxBreadcrumbEditNode;
    procedure SetDisplayName(const AValue: string);
    procedure SetName(const AValue: string);
  protected
    FDisplayName: string;
    FName: string;
    function GetActualDisplayName: string;
    function GetPath: string; virtual;
    procedure ReadData(AStream: TStream; const AVersion: Cardinal = 0); override;
    procedure WriteData(AStream: TStream); override;
    //
    property ActualDisplayName: string read GetActualDisplayName;
    property IsHidden: Boolean read FIsHidden write FIsHidden;
    property PathDelimiter: Char read GetPathDelimiter;
  public
    function AddChild: TdxBreadcrumbEditNode; overload;
    function AddChild(const AName: string; AImageIndex: TcxImageIndex = -1; AData: Pointer = nil): TdxBreadcrumbEditNode; overload;
    function AddChild(const AName, ADisplayName: string; AImageIndex: TcxImageIndex = -1; AData: Pointer = nil): TdxBreadcrumbEditNode; overload;
    function AddChildFirst: TdxBreadcrumbEditNode;
    function AddNode(ANode, ARelative: TdxBreadcrumbEditNode;
      AData: Pointer; AAttachMode: TdxTreeNodeAttachMode): TdxBreadcrumbEditNode;
    function Compare(const AName: string): Boolean; virtual;
    function FindNode(const AName: string; out ANode: TdxBreadcrumbEditNode): Boolean;
    procedure Sort;
    //
    property DisplayName: string read FDisplayName write SetDisplayName;
    property First: TdxBreadcrumbEditNode read GetFirst;
    property Items[Index: Integer]: TdxBreadcrumbEditNode read GetItem; default;
    property Last: TdxBreadcrumbEditNode read GetLast;
    property Name: string read FName write SetName;
    property Next: TdxBreadcrumbEditNode read GetNext;
    property Parent: TdxBreadcrumbEditNode read GetParent;
    property Path: string read GetPath;
    property Prev: TdxBreadcrumbEditNode read GetPrev;
    property Root: TdxBreadcrumbEditNode read GetRoot;
  end;

  { TdxBreadcrumbEditViewItem }

  TdxBreadcrumbEditViewItem = class(TcxIUnknownObject, IdxFadingObject)
  strict private
    FOwner: TdxBreadcrumbEditPartCustomViewInfo;

    function GetController: TdxBreadcrumbEditController;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetUseRightToLeftAlignment: Boolean;
    function GetVisible: Boolean;
  protected
    FBounds: TRect;
    FHotPartIndex: Integer;
    FIsHot: Boolean;
    FIsPressed: Boolean;

    function CalculateAutoHeight: Integer; virtual;
    function CreateFadeImage(AHotPartIndex: Integer; AIsHot, AIsPressed: Boolean): TcxBitmap32; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual; abstract;
    procedure DrawContent(ACanvas: TcxCanvas); virtual; abstract;
    function GetHintText: string; virtual;
    function GetIsEnabled: Boolean; virtual;
    function GetIsHot: Boolean; virtual;
    function GetIsPressed: Boolean; virtual;
    function GetState: TdxBreadcrumbEditButtonState; virtual;
    // IdxFadingObject
    function CanFade: Boolean; virtual;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    procedure IdxFadingObject.DrawFadeImage = Invalidate;
    //
    property Owner: TdxBreadcrumbEditPartCustomViewInfo read FOwner;
  public
    constructor Create(AOwner: TdxBreadcrumbEditPartCustomViewInfo);
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateHitTest(AHitTestInfo: TdxBreadcrumbEditHitTestInfo); virtual;
    procedure Click(APart: Integer = nvipControl); virtual;
    procedure Draw(ACanvas: TcxCanvas);
    procedure Invalidate;
    procedure UpdateStates(AHitTestInfo: TdxBreadcrumbEditHitTestInfo);
    //
    property Bounds: TRect read FBounds;
    property Controller: TdxBreadcrumbEditController read GetController;
    property HintText: string read GetHintText;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TdxBreadcrumbEditButtonState read GetState;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
    property Visible: Boolean read GetVisible;
  end;

  { TdxBreadcrumbEditButtonViewItem }

  TdxBreadcrumbEditButtonViewItem = class(TdxBreadcrumbEditViewItem)
  strict private
    FButton: TdxBreadcrumbEditButton;

    function GetImageSize: TSize;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
  protected
    function CalculateAutoHeight: Integer; override;
    function CalculateAutoWidth: Integer; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    function GetContentOffsets: TRect; virtual;
    function GetHintText: string; override;
    function GetIsEnabled: Boolean; override;
  public
    constructor Create(AOwner: TdxBreadcrumbEditPartCustomViewInfo; AButton: TdxBreadcrumbEditButton); virtual;
    procedure Click(APart: Integer = nvipControl); override;
    //
    property Button: TdxBreadcrumbEditButton read FButton;
    property ContentOffsets: TRect read GetContentOffsets;
    property ImageSize: TSize read GetImageSize;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
  end;

  { TdxBreadcrumbEditNodeViewItem }

  TdxBreadcrumbEditNodeViewItem = class(TdxBreadcrumbEditViewItem)
  private
    FNode: TdxBreadcrumbEditNode;

    function GetButtonRect: TRect;
    function GetDelimiterSize: Integer;
    function GetFont: TFont;
    function GetIndex: Integer;
    function GetTextColor: TColor;
  protected
    FCanHideNode: Boolean;
    FDelimiterRect: TRect;
    FMaxWidth: Integer;
    FMinWidth: Integer;
    FTextSize: TSize;
    FWidth: Integer;

    function CalculateAutoHeight: Integer; override;
    function GetDelimiterState: TdxBreadcrumbEditButtonState; virtual;
    function GetIsPressed: Boolean; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    procedure DrawDelimiterGlyph(ACanvas: TcxCanvas); virtual;
  public
    constructor Create(AOwner: TdxBreadcrumbEditPartCustomViewInfo; ANode: TdxBreadcrumbEditNode); virtual;
    procedure BuildDropDownMenu(AItems: TdxCustomListBoxItems); virtual;
    procedure Calculate(const ABounds: TRect); override;
    procedure CalculateHitTest(AHitTestInfo: TdxBreadcrumbEditHitTestInfo); override;
    procedure CalculateSizes; virtual;
    procedure Click(APart: Integer = nvipControl); override;
    //
    property ButtonRect: TRect read GetButtonRect;
    property CanHideNode: Boolean read FCanHideNode;
    property DelimiterRect: TRect read FDelimiterRect;
    property DelimiterSize: Integer read GetDelimiterSize;
    property DelimiterState: TdxBreadcrumbEditButtonState read GetDelimiterState;
    property Font: TFont read GetFont;
    property Index: Integer read GetIndex;
    property MaxWidth: Integer read FMaxWidth;
    property MinWidth: Integer read FMinWidth;
    property Node: TdxBreadcrumbEditNode read FNode;
    property Width: Integer read FWidth;
  end;

  { TdxBreadcrumbEditRootNodeViewItem }

  TdxBreadcrumbEditRootNodeViewItem = class(TdxBreadcrumbEditNodeViewItem)
  private
    function GetIsLastNode: Boolean;
    function GetIsNextNodeHidden: Boolean;
  protected
    procedure DrawDelimiterGlyph(ACanvas: TcxCanvas); override;
  public
    constructor Create(AOwner: TdxBreadcrumbEditPartCustomViewInfo; ANode: TdxBreadcrumbEditNode); override;
    procedure BuildDropDownMenu(AItems: TdxCustomListBoxItems); override;
    procedure CalculateSizes; override;
    //
    property IsLastNode: Boolean read GetIsLastNode;
    property IsNextNodeHidden: Boolean read GetIsNextNodeHidden;
  end;

  { TdxBreadcrumbEditButton }

  TdxBreadcrumbEditButton = class(TcxCustomEditButton)
  published
    property Action;
    property Enabled;
    property Glyph;
    property Hint;
    property ImageIndex;
    property Tag;
    property Visible;
    property Width;
  end;

  { TdxBreadcrumbEditButtons }

  TdxBreadcrumbEditButtons = class(TcxCustomEditButtons)
  private
    function GetItem(Index: Integer): TdxBreadcrumbEditButton;
    procedure SetItem(Index: Integer; Value: TdxBreadcrumbEditButton);
  protected
    class function GetButtonClass: TcxEditButtonClass; override;
  public
    function Add: TdxBreadcrumbEditButton;
    //
    property Items[Index: Integer]: TdxBreadcrumbEditButton read GetItem write SetItem; default;
  end;

  { TdxBreadcrumbEditRecentPath }

  TdxBreadcrumbEditRecentPath = class(TCollectionItem)
  private
    FImageIndex: TcxImageIndex;
    FPath: string;
    function GetPath: string;
    procedure SetPath(const AValue: string);
  protected
    function GetPathDelimiter: Char;
  public
    constructor Create(Collection: TCollection); override;
  published
    property ImageIndex: TcxImageIndex read FImageIndex write FImageIndex default -1;
    property Path: string read GetPath write SetPath;
  end;

  { TdxBreadcrumbEditRecentPaths }

  TdxBreadcrumbEditRecentPaths = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TdxBreadcrumbEditRecentPath;
    procedure SetItem(AIndex: Integer; AValue: TdxBreadcrumbEditRecentPath);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TdxBreadcrumbEditRecentPath; overload;
    function Add(const APath: string; AImageIndex: TcxImageIndex = -1): TdxBreadcrumbEditRecentPath; overload;
    function IndexOfPath(const APath: string): Integer;
    function Insert(AIndex: Integer): TdxBreadcrumbEditRecentPath; overload;
    function Insert(AIndex: Integer; const APath: string; AImageIndex: TcxImageIndex = -1): TdxBreadcrumbEditRecentPath; overload;
    //
    property Items[Index: Integer]: TdxBreadcrumbEditRecentPath read GetItem write SetItem; default;
  end;

  { TdxBreadcrumbEditCustomPropertiesPersistent }

  TdxBreadcrumbEditCustomPropertiesPersistent = class(TcxOwnedPersistent)
  strict private
    FOnChange: TdxBreadcrumbEditChangeEvent;
  protected
    procedure Changed(AChanges: TdxBreadcrumbEditChanges); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
  public
    property OnChange: TdxBreadcrumbEditChangeEvent read FOnChange write FOnChange;
  end;

  { TdxBreadcrumbEditPathEditorProperties }

  TdxBreadcrumbEditPathEditorProperties = class(TdxBreadcrumbEditCustomPropertiesPersistent)
  strict private
    FAutoComplete: Boolean;
    FAutoCompleteDropDownRows: Integer;
    FEnabled: Boolean;
    FPathDelimiter: Char;
    FReadOnly: Boolean;
    FRecentPaths: TdxBreadcrumbEditRecentPaths;
    FRecentPathsAutoPopulate: Boolean;
    FRecentPathsDropDownRows: Integer;
    FRecentPathsEnabled: Boolean;
    FRecentPathsMaxCount: Integer;

    procedure SetAutoComplete(AValue: Boolean);
    procedure SetAutoCompleteDropDownRows(AValue: Integer);
    procedure SetEnabled(AValue: Boolean);
    procedure SetPathDelimiter(AValue: Char);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetRecentPaths(AValue: TdxBreadcrumbEditRecentPaths);
    procedure SetRecentPathsAutoPopulate(AValue: Boolean);
    procedure SetRecentPathsDropDownRows(AValue: Integer);
    procedure SetRecentPathsEnabled(AValue: Boolean);
    procedure SetRecentPathsMaxCount(AValue: Integer);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete default True;
    property AutoCompleteDropDownRows: Integer read FAutoCompleteDropDownRows
      write SetAutoCompleteDropDownRows default dxBreadcrumbEditDefaultAutoCompleteDropDownRows;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property PathDelimiter: Char read FPathDelimiter write SetPathDelimiter default PathDelim;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RecentPaths: TdxBreadcrumbEditRecentPaths read FRecentPaths write SetRecentPaths;
    property RecentPathsAutoPopulate: Boolean read FRecentPathsAutoPopulate write SetRecentPathsAutoPopulate default True;
    property RecentPathsDropDownRows: Integer read FRecentPathsDropDownRows
      write SetRecentPathsDropDownRows default dxBreadcrumbEditDefaultRecentPathsDropDownRows;
    property RecentPathsEnabled: Boolean read FRecentPathsEnabled write SetRecentPathsEnabled default True;
    property RecentPathsMaxCount: Integer read FRecentPathsMaxCount
      write SetRecentPathsMaxCount default dxBreadcrumbEditDefaultRecentPathsMaxCount;
  end;

  { TdxBreadcrumbEditProgressBarProperties }

  TdxBreadcrumbEditProgressBarCancelEffect = (bpceNone, bpceFillAreaForward, bpceFillAreaBackward);

  TdxBreadcrumbEditProgressBarProperties = class(TdxBreadcrumbEditCustomPropertiesPersistent)
  strict private
    FAnimation: Boolean;
    FAnimationRestartDelay: Cardinal;
    FAnimationSpeed: Cardinal;
    FCancelEffect: TdxBreadcrumbEditProgressBarCancelEffect;
    FMaxValue: Integer;
    FMinValue: Integer;
    FPosition: Integer;

    procedure SetAnimation(AValue: Boolean);
    procedure SetAnimationRestartDelay(AValue: Cardinal);
    procedure SetAnimationSpeed(AValue: Cardinal);
    procedure SetCancelEffect(AValue: TdxBreadcrumbEditProgressBarCancelEffect);
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetPosition(AValue: Integer);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Animation: Boolean read FAnimation write SetAnimation default True;
    property AnimationRestartDelay: Cardinal read FAnimationRestartDelay write SetAnimationRestartDelay default dxBreadcrumbEditDefaultAnimationRestartDelay;
    property AnimationSpeed: Cardinal read FAnimationSpeed write SetAnimationSpeed default dxBreadcrumbEditDefaultAnimationSpeed;
    property CancelEffect: TdxBreadcrumbEditProgressBarCancelEffect read FCancelEffect write SetCancelEffect default bpceFillAreaForward;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 100;
    property MinValue: Integer read FMinValue write SetMinValue default 0;
    property Position: Integer read FPosition write SetPosition default 0;
  end;

  { TdxCustomBreadcrumbEditProperties }

  TdxCustomBreadcrumbEditProperties = class(TdxBreadcrumbEditCustomPropertiesPersistent)
  strict private
    FBorders: TcxBorders;
    FButtonImages: TCustomImageList;
    FButtonImagesChangeLink: TChangeLink;
    FButtons: TdxBreadcrumbEditButtons;
    FDropDownIndent: TdxBreadcrumbEditDropDownIndent;
    FDropDownRows: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FPathEditor: TdxBreadcrumbEditPathEditorProperties;
    FProgressBar: TdxBreadcrumbEditProgressBarProperties;

    procedure ButtonsChanged(Sender: TObject);
    procedure ImageListChanged(Sender: TObject);
    procedure PropertiesChanged(Sender: TObject; AChanges: TdxBreadcrumbEditChanges);
    procedure SetBorders(AValue: TcxBorders);
    procedure SetButtons(AValue: TdxBreadcrumbEditButtons);
    procedure SetButtonImages(AValue: TCustomImageList);
    procedure SetDropDownIndent(AValue: TdxBreadcrumbEditDropDownIndent);
    procedure SetDropDownRows(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetPathEditor(AValue: TdxBreadcrumbEditPathEditorProperties);
    procedure SetProgressBar(AValue: TdxBreadcrumbEditProgressBarProperties);
  protected
    procedure ChangeScale(M, D: Integer); override;
    function CreateButtons: TdxBreadcrumbEditButtons; virtual;
    function CreatePathEditorProperties: TdxBreadcrumbEditPathEditorProperties; virtual;
    function CreateProgressBarProperties: TdxBreadcrumbEditProgressBarProperties; virtual;
    procedure InitiateActions; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    //
    property Borders: TcxBorders read FBorders write SetBorders default cxBordersAll;
    property Buttons: TdxBreadcrumbEditButtons read FButtons write SetButtons;
    property ButtonImages: TCustomImageList read FButtonImages write SetButtonImages;
    property DropDownIndent: TdxBreadcrumbEditDropDownIndent read FDropDownIndent write SetDropDownIndent default ddiNone;
    property DropDownRows: Integer read FDropDownRows write SetDropDownRows default dxBreadcrumbEditDefaultDropDownRows;
    property Images: TCustomImageList read FImages write SetImages;
    property PathEditor: TdxBreadcrumbEditPathEditorProperties read FPathEditor write SetPathEditor;
    property ProgressBar: TdxBreadcrumbEditProgressBarProperties read FProgressBar write SetProgressBar;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxBreadcrumbEditHitTestInfo }

  TdxBreadcrumbEditHitTestInfo = class(TObject)
  public
    Point: TPoint;
    ViewItem: TdxBreadcrumbEditViewItem;
    ViewItemPart: Integer;
    procedure Reset; virtual;
  end;

  { TdxBreadcrumbEditPartCustomViewInfo }

  TdxBreadcrumbEditPartCustomViewInfo = class(TcxIUnknownObject)
  strict private
    FOwner: TdxBreadcrumbEditViewInfo;

    function GetControl: IdxBreadcrumbEdit;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetProperties: TdxCustomBreadcrumbEditProperties;
    function GetScaleFactor: TdxScaleFactor;
    function GetUseRightToLeftAlignment: Boolean;
    function GetViewItem(Index: Integer): TdxBreadcrumbEditViewItem;
    function GetViewItemCount: Integer;
  protected
    FBounds: TRect;
    FViewItemList: TcxObjectList;

    function IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean; virtual;
    procedure InternalDraw(ACanvas: TcxCanvas); virtual;
    function PlaceAt(var R: TRect; AWidth: Integer; AAtRightSide: Boolean): TRect;
  public
    constructor Create(AOwner: TdxBreadcrumbEditViewInfo); virtual;
    destructor Destroy; override;
    procedure Calculate(R: TRect); virtual; abstract;
    function CalculateAutoHeight: Integer; virtual;
    procedure CalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo); virtual;
    procedure Draw(ACanvas: TcxCanvas);
    procedure Invalidate(const R: TRect); overload;
    procedure Invalidate; overload;
    procedure RecreateViewItems; virtual;
    procedure UpdateViewItemsState(AInfo: TdxBreadcrumbEditHitTestInfo); virtual;
    //
    property Bounds: TRect read FBounds;
    property Control: IdxBreadcrumbEdit read GetControl;
    property Owner: TdxBreadcrumbEditViewInfo read FOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property Properties: TdxCustomBreadcrumbEditProperties read GetProperties;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
    property ViewItems[Index: Integer]: TdxBreadcrumbEditViewItem read GetViewItem;
    property ViewItemCount: Integer read GetViewItemCount;
  end;

  { TdxBreadcrumbEditButtonsPartViewInfo }

  TdxBreadcrumbEditButtonsPartViewInfo = class(TdxBreadcrumbEditPartCustomViewInfo)
  protected
    FSeparatorRect: TRect;
    function CreateViewItem(AButton: TdxBreadcrumbEditButton): TdxBreadcrumbEditButtonViewItem; virtual;
    procedure InternalDraw(ACanvas: TcxCanvas); override;
  public
    procedure Calculate(R: TRect); override;
    procedure RecreateViewItems; override;
    //
    property SeparatorRect: TRect read FSeparatorRect;
  end;

  { TdxBreadcrumbEditDropDownButtonViewItem }

  TdxBreadcrumbEditDropDownButtonViewItem = class(TdxBreadcrumbEditViewItem)
  protected
    function CalculateAutoHeight: Integer; override;
    procedure DrawBackground(ACanvas: TcxCanvas); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
  public
    procedure Click(APart: Integer = nvipControl); override;
  end;

  { TdxBreadcrumbEditNodesAreaPartViewInfo }

  TdxBreadcrumbEditNodesAreaPartViewInfo = class(TdxBreadcrumbEditPartCustomViewInfo)
  private
    FDropDownButton: TdxBreadcrumbEditViewItem;

    function GetDropDownButtonWidth: Integer;
    function GetFirstViewItem: TdxBreadcrumbEditNodeViewItem;
    function GetSelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem;
    function GetViewItem(Index: Integer): TdxBreadcrumbEditNodeViewItem;
  protected
    FImageRect: TRect;

    function CreateDropDownButton: TdxBreadcrumbEditViewItem; virtual;
    function CreateViewItem(ANode: TdxBreadcrumbEditNode): TdxBreadcrumbEditNodeViewItem; virtual;
    function GetDefaultImageHeight: Integer; virtual;
    function GetPathEditorRect: TRect; virtual;
    function IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean; override;
    procedure DrawImage(ACanvas: TcxCanvas; const ARect: TRect; AImageIndex: TcxImageIndex); virtual;
    procedure InternalDraw(ACanvas: TcxCanvas); override;
  public
    constructor Create(AOwner: TdxBreadcrumbEditViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(R: TRect); override;
    function CalculateAutoHeight: Integer; override;
    procedure CalculateDropDownButton(var R: TRect); virtual;
    procedure CalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo); override;
    procedure CalculateImageRect(var R: TRect); virtual;
    procedure CalculateNodesBounds(R: TRect); virtual;
    procedure CalculateNodesWidths(AContentWidth: Integer); virtual;
    function FindViewItem(ANode: TdxBreadcrumbEditNode; out AViewItem: TdxBreadcrumbEditNodeViewItem): Boolean;
    procedure RecreateViewItems; override;
    procedure UpdateViewItemsState(AInfo: TdxBreadcrumbEditHitTestInfo); override;
    //
    property DropDownButton: TdxBreadcrumbEditViewItem read FDropDownButton;
    property FirstViewItem: TdxBreadcrumbEditNodeViewItem read GetFirstViewItem;
    property ImageRect: TRect read FImageRect;
    property PathEditorRect: TRect read GetPathEditorRect;
    property SelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem read GetSelectedNodeViewItem;
    property ViewItems[Index: Integer]: TdxBreadcrumbEditNodeViewItem read GetViewItem;
  end;

  { TdxBreadcrumbEditProgressBarPartViewInfo }

  TdxBreadcrumbEditProgressBarPartViewInfo = class(TdxBreadcrumbEditPartCustomViewInfo)
  private
    FOpacity: Byte;
    FOverlayPosition: Integer;
    FOverlaySize: TSize;
    FProgress: Single;
    function GetChunkRect: TRect;
    function GetOverlayRect: TRect;
    procedure SetProgress(AValue: Single);
  protected
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure InternalDraw(ACanvas: TcxCanvas); override;
    //
    property Opacity: Byte read FOpacity write FOpacity;
    property Progress: Single read FProgress write SetProgress;
  public
    procedure Calculate(R: TRect); override;
    procedure ResetOverlayPosition;
    //
    property ChunkRect: TRect read GetChunkRect;
    property OverlayPosition: Integer read FOverlayPosition write FOverlayPosition;
    property OverlayRect: TRect read GetOverlayRect;
    property OverlaySize: TSize read FOverlaySize;
  end;

  { TdxBreadcrumbEditProgressBarController }

  TdxBreadcrumbEditProgressBarFadingState = (bcpfsNone, bcpfsFadeIn, bcpfsFadeOut);

  TdxBreadcrumbEditProgressBarController = class
  private
    FCancelEffectAnimationTimer: TcxTimer;
    FController: TdxBreadcrumbEditController;
    FFadingState: TdxBreadcrumbEditProgressBarFadingState;
    FFadingTimer: TcxTimer;
    FOverlayAnimationTimer: TcxTimer;
    FProgress: Single;

    function GetCancelEffectAnimationActive: Boolean;
    function GetFadingStageAlpha: Byte;
    function GetIsOverlayAnimationEnabled: Boolean;
    function GetOverlayAnimationOffset: Integer;
    function GetOverlayAnimationTimerInterval: Cardinal;
    function GetProperties: TdxBreadcrumbEditProgressBarProperties;
    function GetViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo;
    procedure SetProgress(AValue: Single);
  protected
    FFadingFrameIndex: Integer;
    FFadingFramesCount: Integer;
    FOverlayAnimationRestartDelayCount: Integer;

    procedure Fade(AFramesCount: Integer; AFadeOut: Boolean);
    procedure FadeIn;
    procedure FadeOut;
    procedure ProcessCancelEffectAnimationStep(Sender: TObject); virtual;
    procedure ProcessFadeStep(Sender: TObject); virtual;
    procedure ProcessOverlayAnimationStep(Sender: TObject); virtual;
    procedure StartCancelEffectAnimation;
    procedure StartOverlayAnimation;
    procedure StopCancelEffectAnimation;
    procedure StopFading;
    procedure StopOverlayAnimation;
    //
    property CancelEffectAnimationActive: Boolean read GetCancelEffectAnimationActive;
    property CancelEffectAnimationTimer: TcxTimer read FCancelEffectAnimationTimer;
    property FadingStageAlpha: Byte read GetFadingStageAlpha;
    property FadingState: TdxBreadcrumbEditProgressBarFadingState read FFadingState;
    property FadingTimer: TcxTimer read FFadingTimer;
    property OverlayAnimationEnabled: Boolean read GetIsOverlayAnimationEnabled;
    property OverlayAnimationOffset: Integer read GetOverlayAnimationOffset;
    property OverlayAnimationTimer: TcxTimer read FOverlayAnimationTimer;
    property OverlayAnimationTimerInterval: Cardinal read GetOverlayAnimationTimerInterval;
    property Progress: Single read FProgress write SetProgress;
  public
    constructor Create(AController: TdxBreadcrumbEditController); virtual;
    destructor Destroy; override;
    procedure UpdateOverlayAnimationState; virtual;
    procedure UpdateProgress; virtual;
    //
    property Controller: TdxBreadcrumbEditController read FController;
    property Properties: TdxBreadcrumbEditProgressBarProperties read GetProperties;
    property ViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo read GetViewInfo;
  end;

  { TdxBreadcrumbEditViewInfo }

  TdxBreadcrumbEditViewInfo = class
  private
    FButtonsViewInfo: TdxBreadcrumbEditButtonsPartViewInfo;
    FControl: IdxBreadcrumbEdit;
    FHitTestInfo: TdxBreadcrumbEditHitTestInfo;
    FNodesAreaViewInfo: TdxBreadcrumbEditNodesAreaPartViewInfo;
    FProgressBarViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo;
    FState: TdxBreadcrumbEditState;

    function GetBordersWidth: TRect;
    function GetContentRect: TRect;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetProperties: TdxCustomBreadcrumbEditProperties;
    function GetUseRightToLeftAlignment: Boolean;
    procedure SetState(AState: TdxBreadcrumbEditState);
  protected
    FBounds: TRect;

    function CreateButtonsViewInfo: TdxBreadcrumbEditButtonsPartViewInfo; virtual;
    function CreateHitTestInfo: TdxBreadcrumbEditHitTestInfo; virtual;
    function CreateNodesAreaViewInfo: TdxBreadcrumbEditNodesAreaPartViewInfo; virtual;
    function CreateProgressBarViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo; virtual;
    function IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean; virtual;
    procedure DoCalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo); virtual;
  public
    constructor Create(const AControl: IdxBreadcrumbEdit); virtual;
    destructor Destroy; override;
    procedure Calculate(R: TRect); virtual;
    function CalculateAutoHeight: Integer; virtual;
    procedure CalculateHitTest(const P: TPoint); overload;
    procedure CalculateHitTest(X, Y: Integer); overload;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure GetNavigationOrder(AList: TList); virtual;
    procedure Invalidate(const R: TRect); overload;
    procedure Invalidate; overload;
    procedure UpdateViewItemStates; virtual;
    //
    property BordersWidth: TRect read GetBordersWidth;
    property Bounds: TRect read FBounds;
    property ButtonsViewInfo: TdxBreadcrumbEditButtonsPartViewInfo read FButtonsViewInfo;
    property ContentRect: TRect read GetContentRect;
    property Control: IdxBreadcrumbEdit read FControl;
    property HitTestInfo: TdxBreadcrumbEditHitTestInfo read FHitTestInfo;
    property NodesAreaViewInfo: TdxBreadcrumbEditNodesAreaPartViewInfo read FNodesAreaViewInfo;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ProgressBarViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo read FProgressBarViewInfo;
    property Properties: TdxCustomBreadcrumbEditProperties read GetProperties;
    property State: TdxBreadcrumbEditState read FState write SetState;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxBreadcrumbEditNodeDropDownMenu }

  TdxBreadcrumbEditNodeDropDownMenu = class(TdxCustomDropDownListBox)
  private
    FController: TdxBreadcrumbEditController;
    function GetInnerListBox: TdxBreadcrumbEditDropDownInnerListBox;
    function GetRootAlwaysVisible: Boolean;
  protected
    function CreateInnerListBox: TdxCustomDropDownInnerListBox; override;
    procedure DoCloseUp(AClosedViaKeyboard: Boolean); override;
    procedure DoSelectItem(AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean); override;
    procedure InitInnerListBox; override;
    procedure InitPopup; override;
    procedure InternalCalculateVerticalDirectionPosition(var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize); override;
    //
    property Controller: TdxBreadcrumbEditController read FController;
    property RootAlwaysVisible: Boolean read GetRootAlwaysVisible;
  public
    constructor Create(AController: TdxBreadcrumbEditController); reintroduce; virtual;
    procedure Popup; override;
    //
    property InnerListBox: TdxBreadcrumbEditDropDownInnerListBox read GetInnerListBox;
  end;

  { TdxBreadcrumbEditDropDownInnerListBox }

  TdxBreadcrumbEditDropDownInnerListBox = class(TdxCustomDropDownInnerListBox)
  private
    FRootItem: TdxCustomListBoxItem;
    FRootItemSize: TSize;
    FRootItemState: TcxButtonState;
    function GetIsRootAlwaysVisible: Boolean;
    function GetRootItemRect: TRect;
    procedure SetRootItemState(AValue: TcxButtonState);
  protected
    procedure AdjustItemFont(AFont: TFont; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DoSelectItem(ASelectedViaKeyboard: Boolean); override;
    function GetHasSeparator(AItem: TdxCustomListBoxItem): Boolean; override;
    function GetItemsAreaRect: TRect; override;
    procedure Paint; override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; override;
    procedure SetItemIndex(AIndex: Integer); override;
    //
    property IsRootAlwaysVisible: Boolean read GetIsRootAlwaysVisible;
    property RootItem: TdxCustomListBoxItem read FRootItem;
    property RootItemRect: TRect read GetRootItemRect;
    property RootItemState: TcxButtonState read FRootItemState write SetRootItemState default cxbsNormal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalculateContentSize(AMaxVisibleItemsCount: Integer): TSize; override;
    function ItemAtPos(const APoint: TPoint; AExistOnly: Boolean = False): Integer; override;
    procedure UpdateRoot;
  end;

  { TdxBreadcrumbEditAutoCompleteWindow }

  TdxBreadcrumbEditAutoCompleteWindow = class(TdxCustomAutoCompleteWindow)
  protected
    procedure DoClosed; override;
  end;

  { TdxBreadcrumbEditPathInplaceEditorProperties }

  TdxBreadcrumbEditPathInplaceEditorProperties = class(TcxCustomComboBoxProperties)
  strict private
    FAutoComplete: Boolean;
    FAutoCompleteDropDownRows: Integer;
    FCanShowDropDown: Boolean;
    FImageIndex: TcxImageIndex;

    procedure SetAutoComplete(AValue: Boolean);
    procedure SetAutoCompleteDropDownRows(AValue: Integer);
    procedure SetCanShowDropDown(AValue: Boolean);
    procedure SetImageIndex(AValue: TcxImageIndex);
  protected
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function CanValidate: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    //
    property AutoComplete: Boolean read FAutoComplete write SetAutoComplete;
    property AutoCompleteDropDownRows: Integer read FAutoCompleteDropDownRows write SetAutoCompleteDropDownRows;
    property CanShowDropDown: Boolean read FCanShowDropDown write SetCanShowDropDown;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex;
  end;

  { TdxBreadcrumbEditPathEditorListBox }

  TdxBreadcrumbEditPathEditorListBox = class(TcxComboBoxListBox)
  strict private
    function GetImageIndex(AItemIndex: Integer): TcxImageIndex;
    function GetImageRect(const R: TRect): TRect;
    function GetImages: TCustomImageList;
    function GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    //
    property ImageIndex[AItemIndex: Integer]: TcxImageIndex read GetImageIndex;
    property Images: TCustomImageList read GetImages;
    property Properties: TdxBreadcrumbEditPathInplaceEditorProperties read GetProperties;
  public
    function GetItemHeight(AIndex: Integer = -1): Integer; override;
  end;

  { TdxBreadcrumbEditPathEditorDataBinding }

  TdxBreadcrumbEditPathEditorDataBinding = class(TcxEditDataBinding)
  public
    function CanPostEditorValue: Boolean; override;
  end;

  { TdxBreadcrumbEditPathEditorLookupData }

  TdxBreadcrumbEditPathEditorLookupData = class(TcxComboBoxLookupData)
  private
    function GetCurrentValue: string;
  protected
    function GetListBoxClass: TcxCustomEditListBoxClass; override;
  public
    property CurrentValue: string read GetCurrentValue;
  end;

  { TdxBreadcrumbEditPathEditorViewInfo }

  TdxBreadcrumbEditPathEditorViewInfo = class(TcxCustomComboBoxViewInfo)
  protected
    procedure DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer); override;
    procedure GetColorSettingsByPainter(out ABackgroundColor, ATextColor: TColor); override;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    ImageIndex: TcxImageIndex;
    ImageRect: TRect;
    Images: TCustomImageList;
    procedure Offset(DX: Integer; DY: Integer); override;
  end;

  { TdxBreadcrumbEditPathEditorViewData }

  TdxBreadcrumbEditPathEditorViewData = class(TcxCustomComboBoxViewData)
  private
    function GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
  protected
    procedure CalculateButtonNativeInfo(AButtonViewInfo: TcxEditButtonViewInfo); override;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas;
      AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
      var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    //
    property Properties: TdxBreadcrumbEditPathInplaceEditorProperties read GetProperties;
  end;

  { TdxBreadcrumbEditPathEditor }

  TdxBreadcrumbEditPopulateAutoCompleteSuggestionsEvent = procedure
    (Sender: TObject; const APath: string; ASuggestions: TStringList) of object;

  TdxBreadcrumbEditPathEditor = class(TcxCustomComboBox)
  strict private
    FAutoCompleteSuggestions: TStringList;
    FAutoCompleteSuggestionsUpdateLockCount: Integer;
    FAutoCompleteWindow: TdxBreadcrumbEditAutoCompleteWindow;
    FAutoCompleteWindowCustomizedSize: TSize;

    FOnPopulateAutoCompleteSuggestions: TdxBreadcrumbEditPopulateAutoCompleteSuggestionsEvent;

    function GetActiveProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
    function GetIsAutoCompleteWindowCustomizedSizeAssigned: Boolean;
    function GetLookupData: TdxBreadcrumbEditPathEditorLookupData;
    function GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
    procedure DoAutoCompleteSuggestionsChanged(Sender: TObject);
    procedure DoGetDefaultButtonWidthEvent(Sender: TcxCustomEditViewData; AIndex: Integer; var ADefaultWidth: Integer);
    procedure SelectAutoCompleteWindowItemHandler(Sender: TObject);
    procedure SetAutoCompleteSuggestions(AValue: TStringList);
    procedure StoreAutoCompleteWindowSize(Sender: TObject);
  protected
    function CanDropDown: Boolean; override;
    function CanShowValidationErrorOnPostEditValue: Boolean; override;
    function CreateAutoCompleteWindow: TdxBreadcrumbEditAutoCompleteWindow; virtual;
    function CreateViewData: TcxCustomEditViewData; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function GetScrollLookupDataList(AScrollCause: TcxEditScrollCause): Boolean; override;
    procedure DoChange; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoPopulateAutoCompleteSuggestions; virtual;
    procedure DropDown; override;
    procedure HandleSelectItem(Sender: TObject); override;
    procedure HideAutoCompleteWindow;
    procedure PreviewEditValue(const AEditValue: string; ASelectAll: Boolean);
    procedure RefreshAutoCompleteSuggestions; virtual;
    procedure RefreshAutoCompleteWindow;
    procedure SetEditValue(const AValue: TcxEditValue); override;
    procedure StoreAutoCompleteWindowCustomizedSize; virtual;
    procedure WndProc(var Message: TMessage); override;
    //
    property AutoCompleteWindow: TdxBreadcrumbEditAutoCompleteWindow read FAutoCompleteWindow;
    property AutoCompleteWindowCustomizedSize: TSize read FAutoCompleteWindowCustomizedSize;
    property AutoCompleteWindowCustomizedSizeAssigned: Boolean read GetIsAutoCompleteWindowCustomizedSizeAssigned;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure SetFocusToParent;
    //
    property ActiveProperties: TdxBreadcrumbEditPathInplaceEditorProperties read GetActiveProperties;
    property AutoCompleteSuggestions: TStringList read FAutoCompleteSuggestions write SetAutoCompleteSuggestions;
    property LookupData: TdxBreadcrumbEditPathEditorLookupData read GetLookupData;
    property Properties: TdxBreadcrumbEditPathInplaceEditorProperties read GetProperties;
    //
    property OnPopulateAutoCompleteSuggestions: TdxBreadcrumbEditPopulateAutoCompleteSuggestionsEvent
      read FOnPopulateAutoCompleteSuggestions write FOnPopulateAutoCompleteSuggestions;
  end;

  { TdxBreadcrumbEditPathEditingController }

  TdxBreadcrumbEditPathEditingController = class
  strict private
    FController: TdxBreadcrumbEditController;
    FPathEditor: TdxBreadcrumbEditPathEditor;

    function GetControl: IdxBreadcrumbEdit;
    function GetIsFocused: Boolean;
    function GetProperties: TdxBreadcrumbEditPathEditorProperties;
    function GetRecentPaths: TdxBreadcrumbEditRecentPaths;
    function GetSelected: TdxBreadcrumbEditNode;
    function GetViewInfo: TdxBreadcrumbEditViewInfo;
    procedure PathEditorEnterHandler(Sender: TObject);
    procedure PathEditorExitHandler(Sender: TObject);
    procedure PathEditorPopulateAutoCompleteSuggestions(
      Sender: TObject; const APath: string; ASuggestions: TStringList);
    procedure PathEditorPostEditValueHandler(Sender: TObject);
  protected
    procedure AddToRecentPaths(ANode: TdxBreadcrumbEditNode); virtual;
    function CanAddToRecentPaths(ANode: TdxBreadcrumbEditNode): Boolean; virtual;
    procedure CheckRecentPathsCount; virtual;
    function DoPathEntered(var ANewPath: string): Boolean; virtual;
    procedure PopulateRecentPaths(AItems: TStrings); virtual;
    procedure SetupPathEditorProperties; virtual;
    procedure SetupPathEditorStyles; virtual;
    // Suggestions
    procedure AddSuggestion(const ACurrentPath, ASuggestedPath: string; ASuggestions: TStringList);
    function CanAddNodeToSuggestions(const ACurrentPath: string; ANode: TdxBreadcrumbEditNode): Boolean; virtual;
    function IsSubPath(const ABasePath, APathForCheck: string): Boolean; virtual;
    procedure PopulateCustomSuggestions(const APath: string; ASuggestions: TStringList); virtual;
    procedure PopulateSuggestions(const APath: string; ASuggestions: TStringList); virtual;
  public
    constructor Create(AController: TdxBreadcrumbEditController); virtual;
    destructor Destroy; override;
    procedure ClosePathEditor;
    procedure DropDown;
    procedure OpenPathEditor;
    procedure ReleasePathEditor;
    procedure UpdatePathEditorBounds;
    //
    property Control: IdxBreadcrumbEdit read GetControl;
    property Controller: TdxBreadcrumbEditController read FController;
    property IsFocused: Boolean read GetIsFocused;
    property PathEditor: TdxBreadcrumbEditPathEditor read FPathEditor;
    property Properties: TdxBreadcrumbEditPathEditorProperties read GetProperties;
    property RecentPaths: TdxBreadcrumbEditRecentPaths read GetRecentPaths;
    property Selected: TdxBreadcrumbEditNode read GetSelected;
    property ViewInfo: TdxBreadcrumbEditViewInfo read GetViewInfo;
  end;

  { TdxBreadcrumbEditController }

  TdxBreadcrumbEditController = class(TcxMessageWindow)
  private
    FDestroying: Boolean;
    FDropDownMenuOwner: TdxBreadcrumbEditNodeViewItem;
    FDropDownMenuWindow: TdxBreadcrumbEditNodeDropDownMenu;
    FFocusedViewItem: TdxBreadcrumbEditViewItem;
    FLockDropDownMenuWindowDestroying: Boolean;
    FMouseInControl: Boolean;
    FPathEditingController: TdxBreadcrumbEditPathEditingController;
    FPressedViewItem: TdxBreadcrumbEditViewItem;
    FPrevMousePosition: TPoint;
    FProgressBarController: TdxBreadcrumbEditProgressBarController;
    FSelected: TdxBreadcrumbEditNode;
    FUpdateLockCount: Integer;
    FViewInfo: TdxBreadcrumbEditViewInfo;

    function CanClickAt(AViewItem: TdxBreadcrumbEditViewItem): Boolean;
    function GetControl: IdxBreadcrumbEdit;
    function GetControlContainer: TWinControl;
    function GetHitTestInfo: TdxBreadcrumbEditHitTestInfo;
    function GetIsFocused: Boolean;
    function GetIsUpdateLocked: Boolean;
    function GetRoot: TdxBreadcrumbEditNode;
    function GetSelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem;
    function GetSelectedPath: string;
    procedure SetDropDownMenuOwner(AValue: TdxBreadcrumbEditNodeViewItem);
    procedure SetFocusedViewItem(AValue: TdxBreadcrumbEditViewItem);
    procedure SetMouseInControl(AValue: Boolean);
    procedure SetPressedViewItem(AValue: TdxBreadcrumbEditViewItem);
    procedure SetSelected(AValue: TdxBreadcrumbEditNode);
  protected
    FChanges: TdxBreadcrumbEditChanges;
    function CalculateState: TdxBreadcrumbEditState; virtual;
    function CanOpenPathEditorOnEnter: Boolean; virtual;
    function CanSelectNode(ANode: TdxBreadcrumbEditNode): Boolean; virtual;
    function CreateDropDownMenuWindow: TdxBreadcrumbEditNodeDropDownMenu; virtual;
    function CreatePathEditingController: TdxBreadcrumbEditPathEditingController; virtual;
    function CreateProgressBarController: TdxBreadcrumbEditProgressBarController; virtual;
    function FindNextSelectableViewItem(AItem: TdxBreadcrumbEditViewItem): TdxBreadcrumbEditViewItem; virtual;
    function FindPrevSelectableViewItem(AItem: TdxBreadcrumbEditViewItem): TdxBreadcrumbEditViewItem; virtual;
    function FindRootNodeForPath(var APath: string; out ANode: TdxBreadcrumbEditNode): Boolean; virtual;
    function FindSelectableViewItem(AItem: TdxBreadcrumbEditViewItem; AGoForward: Boolean): TdxBreadcrumbEditViewItem; virtual;
    function ParsePath(var ANodeName, ASubPath: string): Boolean;
    procedure DoDeleteNode(ANode: TdxBreadcrumbEditNode); virtual;
    procedure ProcessChanges(const AChanges: TdxBreadcrumbEditChanges); virtual;
    procedure SelectionChanged; virtual;
    procedure ValidateItem(var AItem: TdxBreadcrumbEditViewItem);
    procedure ValidateItems; virtual;
    procedure ValidatePath(const APath: string; var ANode: TdxBreadcrumbEditNode); virtual;
    procedure WndProc(var Message: TMessage); override;
    //
    procedure DoAfterSelect; virtual;
    procedure DoBeforeSelect; virtual;
    procedure DoNodeDropDownPopup(ANode: TdxBreadcrumbEditNode); virtual;
    function DoSelectPath(const APath: string; ANode: TdxBreadcrumbEditNode): Boolean; virtual;
    procedure DoValidatePath(const APath: string;
      var ANode: TdxBreadcrumbEditNode; var AErrorText: string; var AError: Boolean); virtual;
    //
    property Destroying: Boolean read FDestroying;
    property DropDownMenuOwner: TdxBreadcrumbEditNodeViewItem read FDropDownMenuOwner write SetDropDownMenuOwner;
    property DropDownMenuWindow: TdxBreadcrumbEditNodeDropDownMenu read FDropDownMenuWindow;
    property FocusedViewItem: TdxBreadcrumbEditViewItem read FFocusedViewItem write SetFocusedViewItem;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
    property MouseInControl: Boolean read FMouseInControl write SetMouseInControl;
    property PressedViewItem: TdxBreadcrumbEditViewItem read FPressedViewItem write SetPressedViewItem;
    property ProgressBarController: TdxBreadcrumbEditProgressBarController read FProgressBarController;
    property Root: TdxBreadcrumbEditNode read GetRoot;
    property SelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem read GetSelectedNodeViewItem;
  public
    constructor Create(AViewInfo: TdxBreadcrumbEditViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function FindNodeByPath(APath: string): TdxBreadcrumbEditNode; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BoundsChanged(const ARect: TRect); virtual;
    procedure Changed(const AChanges: TdxBreadcrumbEditChanges);
    procedure FocusChanged; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseDown(AButton: TMouseButton; const P: TPoint; AShift: TShiftState); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(const P: TPoint; AShift: TShiftState); virtual;
    procedure MouseUp(AButton: TMouseButton; const P: TPoint; AShift: TShiftState); virtual;
    procedure ProcessButtonClick(AButton: TdxBreadcrumbEditButton); virtual;
    procedure UpdateHitTest;
    procedure UpdateState;
    // Selection
    function SelectPath(ANode: TdxBreadcrumbEditNode): Boolean; overload;
    function SelectPath(const APath: string): Boolean; overload;
    procedure SelectPathViaDropDownMenu(AItem: TdxBreadcrumbEditNode);
    //
    function IsNodeDropDownMenuWindowActive: Boolean; virtual;
    procedure HideNodeDropDownMenu;
    procedure ShowNodeDropDownMenu(ANodeViewItem: TdxBreadcrumbEditNodeViewItem); virtual;
    //
    property Control: IdxBreadcrumbEdit read GetControl;
    property ControlContainer: TWinControl read GetControlContainer;
    property HitTestInfo: TdxBreadcrumbEditHitTestInfo read GetHitTestInfo;
    property IsFocused: Boolean read GetIsFocused;
    property PathEditingController: TdxBreadcrumbEditPathEditingController read FPathEditingController;
    property Selected: TdxBreadcrumbEditNode read FSelected write SetSelected;
    property SelectedPath: string read GetSelectedPath;
    property ViewInfo: TdxBreadcrumbEditViewInfo read FViewInfo;
  end;

  { TdxCustomBreadcrumbEdit }

  TdxBreadcrumbEditButtonClickEvent = procedure (Sender: TObject; AButton: TdxBreadcrumbEditButton) of object;
  TdxBreadcrumbEditNodeEvent = procedure (Sender: TObject; ANode: TdxBreadcrumbEditNode) of object;
  TdxBreadcrumbEditPathEnteredEvent = procedure (Sender: TObject; var ANewPath: string; var AHandled: Boolean) of object;
  TdxBreadcrumbEditPathValidateEvent = procedure (Sender: TObject; const APath: string;
    var ANode: TdxBreadcrumbEditNode; var AErrorText: string; var AError: Boolean) of object;

  TdxCustomBreadcrumbEdit = class(TcxControl,
    IdxSkinSupport, IdxTreeOwner, IdxBreadcrumbEdit, IdxBreadcrumbEditEvents)
  private
    FController: TdxBreadcrumbEditController;
  {$IFNDEF VCLGLASSPAINT}
    FOnGlass: Boolean;
  {$ENDIF}
    FProperties: TdxCustomBreadcrumbEditProperties;
    FRoot: TdxBreadcrumbEditNode;
    FViewInfo: TdxBreadcrumbEditViewInfo;

    FOnButtonClick: TdxBreadcrumbEditButtonClickEvent;
    FOnDeleteNode: TdxBreadcrumbEditNodeEvent;
    FOnNodeDropDownPopup: TdxBreadcrumbEditNodeEvent;
    FOnPathEntered: TdxBreadcrumbEditPathEnteredEvent;
    FOnPathSelected: TNotifyEvent;
    FOnPathValidate: TdxBreadcrumbEditPathValidateEvent;
    FOnPopulateAutoCompleteSuggestions: TdxBreadcrumbEditPopulateAutoCompleteSuggestionsEvent;
    FOnPopulateChildren: TdxBreadcrumbEditNodeEvent;

    function GetBackgroundColor: TColor;
    function GetSelected: TdxBreadcrumbEditNode;
    procedure PropertiesChanged(Sender: TObject; AChanges: TdxBreadcrumbEditChanges);
    procedure SetProperties(AValue: TdxCustomBreadcrumbEditProperties);
    procedure SetSelected(AValue: TdxBreadcrumbEditNode);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CreateController: TdxBreadcrumbEditController; virtual;
    function CreateProperties: TdxCustomBreadcrumbEditProperties; virtual;
    function CreateRoot: TdxBreadcrumbEditNode; virtual; abstract;
    function CreateViewInfo: TdxBreadcrumbEditViewInfo; virtual;
    procedure Changed(AChanges: TdxBreadcrumbEditChanges); virtual;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;
    procedure CreateWnd; override;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure EnabledChanged; override;
    procedure FocusChanged; override;
    procedure FontChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure RecalculateViewInfo; virtual;
    procedure Resize; override;
    //
    function GetSelectedPath: string; virtual;
    procedure SetSelectedPath(const AValue: string); virtual;
    // IdxTreeOwner
    function CanCollapse(ASender: TdxTreeCustomNode): Boolean;
    function CanExpand(ASender: TdxTreeCustomNode): Boolean;
    procedure Collapsed(ASender: TdxTreeCustomNode);
    procedure Expanded(ASender: TdxTreeCustomNode);
    procedure BeforeDelete(ASender: TdxTreeCustomNode);
    procedure DeleteNode(ASender: TdxTreeCustomNode);
    function GetNodeClass(ARelativeNode: TdxTreeCustomNode): TdxTreeCustomNodeClass;
    function GetTreeOwner: TPersistent;
    function IdxTreeOwner.GetOwner = GetTreeOwner;
    procedure LoadChildren(ASender: TdxTreeCustomNode); virtual;
    procedure TreeNotification(ASender: TdxTreeCustomNode; ANotification: TdxTreeNodeNotifications);
    // IdxBreadcrumbEdit
    function GetContainer: TWinControl;
    function GetController: TdxBreadcrumbEditController;
    function GetFont: TFont;
    function GetIsEnabled: Boolean;
  {$IFNDEF VCLGLASSPAINT}
    function GetOnGlass: Boolean;
  {$ENDIF}
    function GetPathDelimiter: Char;
    function GetProperties: TdxCustomBreadcrumbEditProperties;
    function GetRoot: TdxBreadcrumbEditNode;
    function GetShowHint: Boolean;
    procedure AdjustAutoSize;
    function IdxBreadcrumbEdit.GetLookAndFeel = GetLookAndFeelValue;
    // IdxBreadcrumbEditEvents
    procedure NodeDropDownPopup(ANode: TdxBreadcrumbEditNode);
    procedure PathEntered(var ANewPath: string; var AHandled: Boolean); virtual;
    procedure PathValidate(const APath: string; var ANode: TdxBreadcrumbEditNode;
      var AErrorText: string; var AError: Boolean); virtual;
    procedure PopulateAutoCompleteSuggestions(const APath: string; ASuggestions: TStringList); virtual;
    procedure ProcessButtonClick(AButton: TdxBreadcrumbEditButton); virtual;
    procedure SelectionChanged; virtual;
    //
    property BackgroundColor: TColor read GetBackgroundColor;
    property Controller: TdxBreadcrumbEditController read FController;
    property Root: TdxBreadcrumbEditNode read GetRoot;
    property ViewInfo: TdxBreadcrumbEditViewInfo read FViewInfo;
    //
    property OnButtonClick: TdxBreadcrumbEditButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnDeleteNode: TdxBreadcrumbEditNodeEvent read FOnDeleteNode write FOnDeleteNode;
    property OnNodeDropDownPopup: TdxBreadcrumbEditNodeEvent read FOnNodeDropDownPopup write FOnNodeDropDownPopup;
    property OnPathEntered: TdxBreadcrumbEditPathEnteredEvent read FOnPathEntered write FOnPathEntered;
    property OnPathSelected: TNotifyEvent read FOnPathSelected write FOnPathSelected;
    property OnPathValidate: TdxBreadcrumbEditPathValidateEvent read FOnPathValidate write FOnPathValidate;
    property OnPopulateAutoCompleteSuggestions: TdxBreadcrumbEditPopulateAutoCompleteSuggestionsEvent read FOnPopulateAutoCompleteSuggestions write FOnPopulateAutoCompleteSuggestions;
    property OnPopulateChildren: TdxBreadcrumbEditNodeEvent read FOnPopulateChildren write FOnPopulateChildren;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BrowseParent; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure InitiateAction; override;
    //
    property AutoSize default True;
    property Color default clDefault;
    property LookAndFeel;
  {$IFNDEF VCLGLASSPAINT}
    property OnGlass: Boolean read FOnGlass write FOnGlass default False;
  {$ENDIF}
    property Properties: TdxCustomBreadcrumbEditProperties read GetProperties write SetProperties;
    property Selected: TdxBreadcrumbEditNode read GetSelected write SetSelected;
    property SelectedPath: string read GetSelectedPath write SetSelectedPath;
    property Transparent;
  end;

  { TdxBreadcrumbEditProperties }

  TdxBreadcrumbEditProperties = class(TdxCustomBreadcrumbEditProperties)
  published
    property Borders;
    property Buttons;
    property ButtonImages;
    property DropDownIndent;
    property DropDownRows;
    property Images;
    property PathEditor;
    property ProgressBar;
  end;

  { TdxBreadcrumbEdit }

  TdxBreadcrumbEdit = class(TdxCustomBreadcrumbEdit)
  private
    FLoadedSelectedPath: string;
    function GetProperties: TdxBreadcrumbEditProperties;
    function IsSelectedPathStored: Boolean;
    procedure SetProperties(AValue: TdxBreadcrumbEditProperties);
    procedure ReadNodeData(AStream: TStream);
    procedure ReadOldNodeData(AStream: TStream);
    procedure WriteNodeData(AStream: TStream);
  protected
    function CreateProperties: TdxCustomBreadcrumbEditProperties; override;
    function CreateRoot: TdxBreadcrumbEditNode; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetSelectedPath: string; override;
    procedure Loaded; override;
    procedure SetSelectedPath(const AValue: string); override;
  public
    property Root;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Enabled;
    property Font;
    property LookAndFeel;
    property ParentBiDiMode;
    property ParentShowHint;
    property Properties: TdxBreadcrumbEditProperties read GetProperties write SetProperties;
    property SelectedPath stored IsSelectedPathStored;
    property ShowHint;
    property TabOrder;
    property Transparent;
    property Visible;
    //
    property OnButtonClick;
    property OnDeleteNode;
    property OnNodeDropDownPopup;
    property OnPathEntered;
    property OnPathSelected;
    property OnPathValidate;
    property OnPopulateAutoCompleteSuggestions;
    property OnPopulateChildren;
  end;

implementation

uses
  Math, StdCtrls, cxEditConsts, Variants, StrUtils;

const
  sdxBreadcrumbEditDefaultRootName = 'Root';

  dxBreadcrumbEditButtonsFadeFramesCount = 5;
  dxBreadcrumbEditDefaultImageSize = 16;
  dxBreadcrumbEditImageBorder = 2;
  dxBreadcrumbEditMenuMaxWidth = 260;
  dxBreadcrumbEditMenuMinWidth = 180;
  dxBreadcrumbEditMinReducedNodeWidth = 60;
  dxBreadcrumbEditMinVisibleNodesCount = 2;
  dxBreadcrumbEditProgressFadeInFramesCount = 30;
  dxBreadcrumbEditProgressFadeOutFramesCount = 15;
  dxBreadcrumbEditProgressFillingDelta = 7;
  dxBreadcrumbEditSuggestionsWindowBorderSize = 1;

function CompareNodes(ANode1, ANode2: TdxBreadcrumbEditNode): Integer;
begin
  Result := CompareStr(ANode1.Name, ANode2.Name);
end;

function ValidateProgressValue(AValue: Single): Single;
begin
  Result := Max(0, Min(100, AValue));
end;

{ TdxBreadcrumbEditNode }

function TdxBreadcrumbEditNode.AddChild: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited AddChild);
end;

function TdxBreadcrumbEditNode.AddChild(const AName: string;
  AImageIndex: TcxImageIndex = -1; AData: Pointer = nil): TdxBreadcrumbEditNode;
begin
  Result := AddChild(AName, '', AImageIndex, AData);
end;

function TdxBreadcrumbEditNode.AddChild(const AName, ADisplayName: string;
  AImageIndex: TcxImageIndex = -1; AData: Pointer = nil): TdxBreadcrumbEditNode;
begin
  FOwner.BeginUpdate;
  try
    Result := AddChild;
    Result.Name := AName;
    Result.DisplayName := ADisplayName;
    Result.ImageIndex := AImageIndex;
    Result.Data := AData;
  finally
    FOwner.EndUpdate;
  end;
end;

function TdxBreadcrumbEditNode.AddChildFirst: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited AddChildFirst);
end;

function TdxBreadcrumbEditNode.AddNode(ANode, ARelative: TdxBreadcrumbEditNode;
  AData: Pointer; AAttachMode: TdxTreeNodeAttachMode): TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited AddNode(ANode, ARelative, AData, AAttachMode));
end;

function TdxBreadcrumbEditNode.Compare(const AName: string): Boolean;
begin
  Result := SameText(Name, AName) or SameText(ActualDisplayName, AName);
end;

function TdxBreadcrumbEditNode.FindNode(
  const AName: string; out ANode: TdxBreadcrumbEditNode): Boolean;
var
  I: Integer;
begin
  Result := False;
  LoadChildren;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Compare(AName);
    if Result then
    begin
      ANode := Items[I];
      Break;
    end;
  end;
end;

procedure TdxBreadcrumbEditNode.ReadData(AStream: TStream; const AVersion: Cardinal = 0);
var
  AIsUnicodeStream: Boolean;
begin
  AIsUnicodeStream := dxIsUnicodeStream(AStream);
  Name := dxReadStr(AStream, AIsUnicodeStream);
  if AVersion = dxBreadcrumbEditStreamVersion then
    DisplayName := dxReadStr(AStream, AIsUnicodeStream);
  inherited ReadData(AStream, AVersion);
end;

procedure TdxBreadcrumbEditNode.WriteData(AStream: TStream);
begin
  dxWriteStreamType(AStream);
  dxWriteStr(AStream, Name);
  dxWriteStr(AStream, DisplayName);
  inherited WriteData(AStream);
end;

procedure TdxBreadcrumbEditNode.Sort;
begin
  CustomSort(@CompareNodes);
end;

function TdxBreadcrumbEditNode.GetFirst: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited First);
end;

function TdxBreadcrumbEditNode.GetItem(AIndex: Integer): TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Items[AIndex]);
end;

function TdxBreadcrumbEditNode.GetLast: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Last);
end;

function TdxBreadcrumbEditNode.GetNext: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Next);
end;

function TdxBreadcrumbEditNode.GetParent: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Parent);
end;

function TdxBreadcrumbEditNode.GetActualDisplayName: string;
begin
  if DisplayName = '' then
    Result := Name
  else
    Result := DisplayName;
end;

function TdxBreadcrumbEditNode.GetPath: string;
begin
  if Parent <> nil then
    Result := Parent.Path + PathDelimiter + Name
  else
    Result := Name;
end;

function TdxBreadcrumbEditNode.GetPathDelimiter: Char;
var
  ABreadcrumbEdit: IdxBreadcrumbEdit;
begin
  if Supports(Owner, IdxBreadcrumbEdit, ABreadcrumbEdit) then
    Result := ABreadcrumbEdit.GetPathDelimiter
  else
    Result := PathDelim;
end;

function TdxBreadcrumbEditNode.GetPrev: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Prev);
end;

function TdxBreadcrumbEditNode.GetRoot: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode(inherited Root);
end;

procedure TdxBreadcrumbEditNode.SetDisplayName(const AValue: string);
begin
  if AValue <> FDisplayName then
  begin
    FDisplayName := AValue;
    Notify([tnData]);
  end;
end;

procedure TdxBreadcrumbEditNode.SetName(const AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
    Notify([tnData]);
  end;
end;

{ TdxBreadcrumbEditViewItem }

constructor TdxBreadcrumbEditViewItem.Create(AOwner: TdxBreadcrumbEditPartCustomViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TdxBreadcrumbEditViewItem.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

procedure TdxBreadcrumbEditViewItem.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

function TdxBreadcrumbEditViewItem.CalculateAutoHeight: Integer;
begin
  Result := 0;
end;

procedure TdxBreadcrumbEditViewItem.CalculateHitTest(AHitTestInfo: TdxBreadcrumbEditHitTestInfo);
begin
  if PtInRect(Bounds, AHitTestInfo.Point) then
  begin
    AHitTestInfo.ViewItem := Self;
    AHitTestInfo.ViewItemPart := nvipControl;
  end;
end;

function TdxBreadcrumbEditViewItem.CanFade: Boolean;
begin
  Result := Painter.BreadcrumbEditIsFadingSupports;
end;

procedure TdxBreadcrumbEditViewItem.Click(APart: Integer = nvipControl);
begin
end;

function TdxBreadcrumbEditViewItem.CreateFadeImage(
  AHotPartIndex: Integer; AIsHot, AIsPressed: Boolean): TcxBitmap32;
var
  APrevHot: Boolean;
  APrevHotPartIndex: Integer;
  APrevPressed: Boolean;
begin
  APrevHot := FIsHot;
  APrevHotPartIndex := FHotPartIndex;
  APrevPressed := FIsPressed;
  try
    FIsHot := AIsHot;
    FIsPressed := AIsPressed;
    FHotPartIndex := AHotPartIndex;
    Result := TcxBitmap32.CreateSize(Bounds, True);
    Result.cxCanvas.WindowOrg := Bounds.TopLeft;
    DrawBackground(Result.cxCanvas);
    Result.cxCanvas.WindowOrg := cxNullPoint;
  finally
    FHotPartIndex := APrevHotPartIndex;
    FIsPressed := APrevPressed;
    FIsHot := APrevHot;
  end;
end;

procedure TdxBreadcrumbEditViewItem.Draw(ACanvas: TcxCanvas);
begin
  if Visible then
  begin
    if not dxFader.DrawFadeImage(Self, ACanvas.Handle, Bounds) then
      DrawBackground(ACanvas);
    DrawContent(ACanvas);
  end;
end;

procedure TdxBreadcrumbEditViewItem.Invalidate;
begin
  Owner.Invalidate(Bounds);
end;

procedure TdxBreadcrumbEditViewItem.UpdateStates(AHitTestInfo: TdxBreadcrumbEditHitTestInfo);

  function GetHotPartIndex: Integer;
  begin
    if Self = AHitTestInfo.ViewItem then
      Result := AHitTestInfo.ViewItemPart
    else
      Result := nvipNone;
  end;

  function DoUpdateState(AHotPartIndex: Integer; AIsHot, AIsPressed: Boolean): Boolean;
  var
    APrevHotPartIndex: Integer;
    APrevState: TdxBreadcrumbEditButtonState;
  begin
    APrevState := State;
    APrevHotPartIndex := FHotPartIndex;
    FHotPartIndex := AHotPartIndex;
    FIsPressed := AIsPressed;
    FIsHot := AIsHot;
    Result := (State <> APrevState) or (State = dxbcbsHot) and (APrevHotPartIndex <> FHotPartIndex);
  end;

var
  APrevHot: Boolean;
  APrevHotPartIndex: Integer;
  APrevPressed: Boolean;
begin
  APrevHot := FIsHot;
  APrevPressed := FIsPressed;
  APrevHotPartIndex := FHotPartIndex;
  if DoUpdateState(GetHotPartIndex, GetIsHot, GetIsPressed) then
  begin
    if CanFade then
    begin
      dxFader.Fade(Self,
        CreateFadeImage(APrevHotPartIndex, APrevHot, APrevPressed),
        CreateFadeImage(GetHotPartIndex, GetIsHot, GetIsPressed),
        dxBreadcrumbEditButtonsFadeFramesCount, dxFadeOutDefaultAnimationFrameDelay);
    end;
    Invalidate;
  end;
end;

function TdxBreadcrumbEditViewItem.GetController: TdxBreadcrumbEditController;
begin
  Result := Owner.Control.GetController;
end;

procedure TdxBreadcrumbEditViewItem.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
begin
  AFadeOutImage := nil;
  AFadeInImage := nil;
end;

function TdxBreadcrumbEditViewItem.GetHintText: string;
begin
  Result := '';
end;

function TdxBreadcrumbEditViewItem.GetIsEnabled: Boolean;
begin
  Result := Owner.Owner.State <> dxbcsDisabled;
end;

function TdxBreadcrumbEditViewItem.GetIsHot: Boolean;
begin
  Result := (Controller.FocusedViewItem = Self) and (Controller.PressedViewItem = nil);
end;

function TdxBreadcrumbEditViewItem.GetIsPressed: Boolean;
begin
  Result := (Controller.FocusedViewItem = Self) and (Controller.PressedViewItem = Self);
end;

function TdxBreadcrumbEditViewItem.GetState: TdxBreadcrumbEditButtonState;
const
  HotStateMap: array[Boolean] of TdxBreadcrumbEditButtonState = (dxbcbsHot, dxbcbsFocused);
begin
  if not GetIsEnabled then
    Result := dxbcbsDisabled
  else
    if FIsPressed then
      Result := dxbcbsPressed
    else
      if FIsHot then
        Result := HotStateMap[FHotPartIndex <> nvipControl]
      else
        Result := dxbcbsNormal;
end;

function TdxBreadcrumbEditViewItem.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Owner.Control.UseRightToLeftAlignment;
end;

function TdxBreadcrumbEditViewItem.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.Control.GetLookAndFeel.Painter;
end;

function TdxBreadcrumbEditViewItem.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.Control.GetScaleFactor;
end;

function TdxBreadcrumbEditViewItem.GetVisible: Boolean;
begin
  Result := not cxRectIsEmpty(Bounds);
end;

{ TdxBreadcrumbEditButtonViewItem }

constructor TdxBreadcrumbEditButtonViewItem.Create(
  AOwner: TdxBreadcrumbEditPartCustomViewInfo; AButton: TdxBreadcrumbEditButton);
begin
  inherited Create(AOwner);
  FButton := AButton;
end;

function TdxBreadcrumbEditButtonViewItem.CalculateAutoHeight: Integer;
begin
  Result := cxMarginsHeight(ContentOffsets) + ImageSize.cy;
end;

function TdxBreadcrumbEditButtonViewItem.CalculateAutoWidth: Integer;
begin
  Result := cxMarginsWidth(ContentOffsets) + ImageSize.cx;
end;

procedure TdxBreadcrumbEditButtonViewItem.Click(APart: Integer = nvipControl);
begin
  Controller.ProcessButtonClick(Button);
end;

procedure TdxBreadcrumbEditButtonViewItem.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawBreadcrumbEditScaledButton(ACanvas, Bounds, State, IsFirst, IsLast, ScaleFactor);
end;

procedure TdxBreadcrumbEditButtonViewItem.DrawContent(ACanvas: TcxCanvas);
begin
  cxDrawImage(ACanvas.Handle,
    cxRectCenter(cxRectContent(Bounds, ContentOffsets), ImageSize),
    Bounds, Button.Glyph, Owner.Properties.ButtonImages,
    Button.ImageIndex, EnabledImageDrawModeMap[GetIsEnabled], False, 0, clDefault, False,
    Painter.BreadcrumbEditButtonColorPalette(State));
end;

function TdxBreadcrumbEditButtonViewItem.GetContentOffsets: TRect;
begin
  Result := Painter.BreadcrumbEditScaledButtonContentOffsets(IsFirst, IsLast, ScaleFactor);
end;

function TdxBreadcrumbEditButtonViewItem.GetHintText: string;
begin
  Result := Button.Hint;
  if Button.ActionLink <> nil then
    Button.ActionLink.PrepareHint(Result);
end;

function TdxBreadcrumbEditButtonViewItem.GetImageSize: TSize;
begin
  if IsImageAssigned(Button.Glyph, Owner.Properties.ButtonImages, Button.ImageIndex) then
    Result := dxGetImageSize(Button.Glyph, Owner.Properties.ButtonImages, Button.ImageIndex, ScaleFactor)
  else
    Result := ScaleFactor.Apply(cxSize(dxBreadcrumbEditDefaultImageSize));
end;

function TdxBreadcrumbEditButtonViewItem.GetIsFirst: Boolean;
begin
  Result := Self = Owner.FViewItemList.First;
end;

function TdxBreadcrumbEditButtonViewItem.GetIsEnabled: Boolean;
begin
  Result := Button.Enabled and inherited GetIsEnabled;
end;

function TdxBreadcrumbEditButtonViewItem.GetIsLast: Boolean;
begin
  Result := Self = Owner.FViewItemList.Last;
end;

{ TdxBreadcrumbEditNodeViewItem }

constructor TdxBreadcrumbEditNodeViewItem.Create(
  AOwner: TdxBreadcrumbEditPartCustomViewInfo; ANode: TdxBreadcrumbEditNode);
begin
  inherited Create(AOwner);
  FNode := ANode;
  FCanHideNode := True;
end;

procedure TdxBreadcrumbEditNodeViewItem.BuildDropDownMenu(AItems: TdxCustomListBoxItems);
var
  ANode: TdxBreadcrumbEditNode;
begin
  Node.LoadChildren;
  ANode := Node.First;
  while ANode <> nil do
  begin
    if not ANode.IsHidden then
      AItems.Add(ANode.ActualDisplayName, ANode.ImageIndex, ANode);
    ANode := ANode.Next;
  end;
end;

procedure TdxBreadcrumbEditNodeViewItem.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);

  if Visible then
  begin
    if UseRightToLeftAlignment then
      FDelimiterRect := cxRectSetWidth(Bounds, DelimiterSize)
    else
      FDelimiterRect := cxRectSetRight(Bounds, Bounds.Right, DelimiterSize);
  end
  else
    FDelimiterRect := cxNullRect;
end;

function TdxBreadcrumbEditNodeViewItem.CalculateAutoHeight: Integer;
begin
  Result := FTextSize.cy + cxMarginsHeight(Painter.BreadcrumbEditScaledNodeTextOffsets(ScaleFactor));
end;

procedure TdxBreadcrumbEditNodeViewItem.CalculateHitTest(AHitTestInfo: TdxBreadcrumbEditHitTestInfo);
begin
  inherited CalculateHitTest(AHitTestInfo);
  if PtInRect(DelimiterRect, AHitTestInfo.Point) then
    AHitTestInfo.ViewItemPart := nvipDelimiter;
end;

procedure TdxBreadcrumbEditNodeViewItem.CalculateSizes;
begin
  FTextSize := cxTextExtent(Font, Node.ActualDisplayName);
  FMaxWidth := DelimiterSize + Max(Painter.BreadcrumbEditScaledNodeDelimiterSize(ScaleFactor),
    FTextSize.cx + cxMarginsWidth(Painter.BreadcrumbEditScaledNodeTextOffsets(ScaleFactor)));
  FMinWidth := Min(ScaleFactor.Apply(dxBreadcrumbEditMinReducedNodeWidth), MaxWidth);
  FWidth := MaxWidth;
end;

procedure TdxBreadcrumbEditNodeViewItem.Click(APart: Integer = nvipControl);
begin
  if APart = nvipControl then
    Controller.SelectPath(Node);
end;

procedure TdxBreadcrumbEditNodeViewItem.DrawBackground(ACanvas: TcxCanvas);
begin
  if cxRectIsEmpty(ButtonRect) then
    Painter.DrawBreadcrumbEditScaledNode(ACanvas, DelimiterRect, DelimiterState, False, ScaleFactor)
  else
    if cxRectIsEmpty(DelimiterRect) then
      Painter.DrawBreadcrumbEditScaledNode(ACanvas, ButtonRect, State, False, ScaleFactor)
    else
    begin
      Painter.DrawBreadcrumbEditScaledNode(ACanvas, ButtonRect, State, True, ScaleFactor);
      Painter.DrawBreadcrumbEditScaledNodeDelimiter(ACanvas, DelimiterRect, DelimiterState, ScaleFactor);
    end;
end;

procedure TdxBreadcrumbEditNodeViewItem.DrawContent(ACanvas: TcxCanvas);
const
  AlignMap: array[Boolean] of TAlignment = (taLeftJustify, taCenter);
var
  R: TRect;
begin
  DrawDelimiterGlyph(ACanvas);
  R := cxRectContent(ButtonRect, Painter.BreadcrumbEditScaledNodeTextOffsets(ScaleFactor));
  if not cxRectIsEmpty(R) then
  begin
    ACanvas.Font := Font;
    ACanvas.Font.Color := GetTextColor;
    ACanvas.DrawTexT(Node.ActualDisplayName, R, AlignMap[cxRectWidth(R) > FTextSize.cx], vaCenter, False, True);
  end;
end;

procedure TdxBreadcrumbEditNodeViewItem.DrawDelimiterGlyph(ACanvas: TcxCanvas);
begin
  Painter.DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas, DelimiterRect, DelimiterState, ScaleFactor);
end;

function TdxBreadcrumbEditNodeViewItem.GetButtonRect: TRect;
begin
  Result := Bounds;
  if UseRightToLeftAlignment then
    Result.Left := DelimiterRect.Right
  else
    Result.Right := DelimiterRect.Left;
end;

function TdxBreadcrumbEditNodeViewItem.GetDelimiterSize: Integer;
begin
  if Node.HasChildren then
    Result := Painter.BreadcrumbEditScaledNodeDelimiterSize(ScaleFactor)
  else
    Result := 0;
end;

function TdxBreadcrumbEditNodeViewItem.GetDelimiterState: TdxBreadcrumbEditButtonState;
const
  HotStateMap: array[Boolean] of TdxBreadcrumbEditButtonState = (dxbcbsHot, dxbcbsFocused);
begin
  if not GetIsEnabled then
    Result := dxbcbsDisabled
  else
    if FIsPressed then
      Result := dxbcbsPressed
    else
      if FIsHot then
        Result := HotStateMap[FHotPartIndex = nvipNone]
      else
        Result := dxbcbsNormal;
end;

function TdxBreadcrumbEditNodeViewItem.GetFont: TFont;
begin
  Result := Owner.Control.GetFont;
end;

function TdxBreadcrumbEditNodeViewItem.GetIndex: Integer;
begin
  Result := Owner.FViewItemList.IndexOf(Self);
end;

function TdxBreadcrumbEditNodeViewItem.GetIsPressed: Boolean;
begin
  Result := (Self = Controller.DropDownMenuOwner) or inherited GetIsPressed;
end;

function TdxBreadcrumbEditNodeViewItem.GetTextColor: TColor;
begin
  Result := Font.Color;
  if Result = clWindowText then
    Result := Painter.BreadcrumbEditNodeTextColor(State);
  if Result = clDefault then
    Result := clWindowText;
end;

{ TdxBreadcrumbEditRootNodeViewItem }

constructor TdxBreadcrumbEditRootNodeViewItem.Create(
  AOwner: TdxBreadcrumbEditPartCustomViewInfo; ANode: TdxBreadcrumbEditNode);
begin
  inherited Create(AOwner, ANode);
  FCanHideNode := False;
end;

procedure TdxBreadcrumbEditRootNodeViewItem.BuildDropDownMenu(AItems: TdxCustomListBoxItems);

  procedure PopulateHiddenNodes(AItems: TdxCustomListBoxItems);
  var
    AIndex: Integer;
    ANode: TdxBreadcrumbEditNode;
  begin
    AIndex := Index + 1;
    while AIndex < Owner.ViewItemCount do
    begin
      if Owner.ViewItems[AIndex].Visible then
        Break;
      ANode := (Owner.ViewItems[AIndex] as TdxBreadcrumbEditNodeViewItem).Node;
      AItems.Insert(0, ANode.ActualDisplayName, ANode.ImageIndex, ANode);
      Inc(AIndex);
    end;
  end;

begin
  PopulateHiddenNodes(AItems);
  AItems.Add(Node.ActualDisplayName, Node.ImageIndex, Node);
  inherited BuildDropDownMenu(AItems);
end;

procedure TdxBreadcrumbEditRootNodeViewItem.CalculateSizes;
begin
  inherited CalculateSizes;
  if not IsLastNode then
    FMaxWidth := Painter.BreadcrumbEditScaledNodeDelimiterSize(ScaleFactor);
  FMinWidth := MaxWidth;
  FWidth := MaxWidth;
end;

procedure TdxBreadcrumbEditRootNodeViewItem.DrawDelimiterGlyph(ACanvas: TcxCanvas);
begin
  if IsNextNodeHidden then
    Painter.DrawBreadcrumbEditScaledNodeMoreButtonGlyph(ACanvas, DelimiterRect, DelimiterState, ScaleFactor)
  else
    inherited DrawDelimiterGlyph(ACanvas);
end;

function TdxBreadcrumbEditRootNodeViewItem.GetIsNextNodeHidden: Boolean;
begin
  Result := not (IsLastNode or Owner.ViewItems[Index + 1].Visible);
end;

function TdxBreadcrumbEditRootNodeViewItem.GetIsLastNode: Boolean;
begin
  Result := Index + 1 = Owner.ViewItemCount;
end;

{ TdxBreadcrumbEditButtons }

function TdxBreadcrumbEditButtons.Add: TdxBreadcrumbEditButton;
begin
  Result := TdxBreadcrumbEditButton(inherited Add);
end;

class function TdxBreadcrumbEditButtons.GetButtonClass: TcxEditButtonClass;
begin
  Result := TdxBreadcrumbEditButton;
end;

function TdxBreadcrumbEditButtons.GetItem(Index: Integer): TdxBreadcrumbEditButton;
begin
  Result := TdxBreadcrumbEditButton(inherited GetItem(Index));
end;

procedure TdxBreadcrumbEditButtons.SetItem(Index: Integer; Value: TdxBreadcrumbEditButton);
begin
  inherited SetItem(Index, Value);
end;

{ TdxBreadcrumbEditCustomPropertiesPersistent }

procedure TdxBreadcrumbEditCustomPropertiesPersistent.Changed(AChanges: TdxBreadcrumbEditChanges);
begin
  if Assigned(OnChange) then OnChange(Self, AChanges);
end;

procedure TdxBreadcrumbEditCustomPropertiesPersistent.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

{ TdxBreadcrumbEditProgressBarProperties }

constructor TdxBreadcrumbEditProgressBarProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAnimation := True;
  FAnimationRestartDelay := dxBreadcrumbEditDefaultAnimationRestartDelay;
  FAnimationSpeed := dxBreadcrumbEditDefaultAnimationSpeed;
  FCancelEffect := bpceFillAreaForward;
  FMaxValue := 100;
end;

procedure TdxBreadcrumbEditProgressBarProperties.Assign(Source: TPersistent);
begin
  if Source is TdxBreadcrumbEditProgressBarProperties then
  begin
    Animation := TdxBreadcrumbEditProgressBarProperties(Source).Animation;
    AnimationRestartDelay := TdxBreadcrumbEditProgressBarProperties(Source).AnimationRestartDelay;
    AnimationSpeed := TdxBreadcrumbEditProgressBarProperties(Source).AnimationSpeed;
    CancelEffect := TdxBreadcrumbEditProgressBarProperties(Source).CancelEffect;
    MaxValue := TdxBreadcrumbEditProgressBarProperties(Source).MaxValue;
    MinValue := TdxBreadcrumbEditProgressBarProperties(Source).MinValue;
    Position := TdxBreadcrumbEditProgressBarProperties(Source).Position;
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetAnimation(AValue: Boolean);
begin
  if AValue <> FAnimation then
  begin
    FAnimation := AValue;
    Changed([bcecAnimation]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetAnimationRestartDelay(AValue: Cardinal);
begin
  if AValue <> FAnimationRestartDelay then
  begin
    FAnimationRestartDelay := AValue;
    if Animation then
      Changed([bcecAnimation]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetAnimationSpeed(AValue: Cardinal);
begin
  if AValue <> FAnimationSpeed then
  begin
    FAnimationSpeed := AValue;
    if Animation then
      Changed([bcecAnimation]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetCancelEffect(
  AValue: TdxBreadcrumbEditProgressBarCancelEffect);
begin
  if AValue <> FCancelEffect then
  begin
    FCancelEffect := AValue;
    Changed([bcecAnimation]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetMaxValue(AValue: Integer);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    FMinValue := Min(MinValue, MaxValue);
    FPosition := Min(MaxValue, Position);
    Changed([bcecProgress]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetMinValue(AValue: Integer);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    FMaxValue := Max(MinValue, MaxValue);
    FPosition := Max(MinValue, Position);
    Changed([bcecProgress]);
  end;
end;

procedure TdxBreadcrumbEditProgressBarProperties.SetPosition(AValue: Integer);
begin
  AValue := Max(MinValue, Min(AValue, MaxValue));
  if AValue <> FPosition then
  begin
    FPosition := AValue;
    Changed([bcecProgress]);
  end;
end;

{ TdxBreadcrumbEditRecentPath }

constructor TdxBreadcrumbEditRecentPath.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
end;

function TdxBreadcrumbEditRecentPath.GetPathDelimiter: Char;
begin
  if Collection.Owner is TdxBreadcrumbEditPathEditorProperties then
    Result := TdxBreadcrumbEditPathEditorProperties(Collection.Owner).PathDelimiter
  else
    Result := PathDelim;
end;

function TdxBreadcrumbEditRecentPath.GetPath: string;
begin
  Result := dxReplacePathDelimiter(FPath, PathDelim, GetPathDelimiter);
end;

procedure TdxBreadcrumbEditRecentPath.SetPath(const AValue: string);
begin
  FPath := dxReplacePathDelimiter(AValue, GetPathDelimiter, PathDelim);
end;

{ TdxBreadcrumbEditRecentPaths }

constructor TdxBreadcrumbEditRecentPaths.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TdxBreadcrumbEditRecentPath);
end;

function TdxBreadcrumbEditRecentPaths.Add: TdxBreadcrumbEditRecentPath;
begin
  Result := TdxBreadcrumbEditRecentPath(inherited Add);
end;

function TdxBreadcrumbEditRecentPaths.Add(
  const APath: string; AImageIndex: TcxImageIndex = -1): TdxBreadcrumbEditRecentPath;
begin
  BeginUpdate;
  try
    Result := Add;
    Result.Path := APath;
    Result.ImageIndex := AImageIndex;
  finally
    EndUpdate;
  end;
end;

function TdxBreadcrumbEditRecentPaths.IndexOfPath(const APath: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SameText(Items[I].Path, APath) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxBreadcrumbEditRecentPaths.Insert(AIndex: Integer): TdxBreadcrumbEditRecentPath;
begin
  Result := TdxBreadcrumbEditRecentPath(inherited Insert(AIndex));
end;

function TdxBreadcrumbEditRecentPaths.Insert(AIndex: Integer;
  const APath: string; AImageIndex: TcxImageIndex = -1): TdxBreadcrumbEditRecentPath;
begin
  BeginUpdate;
  try
    Result := Insert(AIndex);
    Result.Path := APath;
    Result.ImageIndex := AImageIndex;
  finally
    EndUpdate;
  end;
end;

function TdxBreadcrumbEditRecentPaths.GetItem(AIndex: Integer): TdxBreadcrumbEditRecentPath;
begin
  Result := TdxBreadcrumbEditRecentPath(inherited Items[AIndex]);
end;

procedure TdxBreadcrumbEditRecentPaths.SetItem(AIndex: Integer; AValue: TdxBreadcrumbEditRecentPath);
begin
  inherited Items[AIndex] := AValue;
end;

{ TdxBreadcrumbEditPathEditorProperties }

constructor TdxBreadcrumbEditPathEditorProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FAutoComplete := True;
  FAutoCompleteDropDownRows := dxBreadcrumbEditDefaultAutoCompleteDropDownRows;
  FPathDelimiter := PathDelim;
  FRecentPaths := TdxBreadcrumbEditRecentPaths.Create(Self);
  FRecentPathsDropDownRows := dxBreadcrumbEditDefaultRecentPathsDropDownRows;
  FRecentPathsAutoPopulate := True;
  FRecentPathsMaxCount := dxBreadcrumbEditDefaultRecentPathsMaxCount;
  FRecentPathsEnabled := True;
end;

destructor TdxBreadcrumbEditPathEditorProperties.Destroy;
begin
  FreeAndNil(FRecentPaths);
  inherited Destroy;
end;

procedure TdxBreadcrumbEditPathEditorProperties.Assign(Source: TPersistent);
begin
  if Source is TdxBreadcrumbEditPathEditorProperties then
  begin
    AutoComplete := TdxBreadcrumbEditPathEditorProperties(Source).AutoComplete;
    AutoCompleteDropDownRows := TdxBreadcrumbEditPathEditorProperties(Source).AutoCompleteDropDownRows;
    Enabled := TdxBreadcrumbEditPathEditorProperties(Source).Enabled;
    PathDelimiter := TdxBreadcrumbEditPathEditorProperties(Source).PathDelimiter;
    ReadOnly := TdxBreadcrumbEditPathEditorProperties(Source).ReadOnly;
    RecentPaths := TdxBreadcrumbEditPathEditorProperties(Source).RecentPaths;
    RecentPathsAutoPopulate := TdxBreadcrumbEditPathEditorProperties(Source).RecentPathsAutoPopulate;
    RecentPathsDropDownRows := TdxBreadcrumbEditPathEditorProperties(Source).RecentPathsDropDownRows;
    RecentPathsMaxCount := TdxBreadcrumbEditPathEditorProperties(Source).RecentPathsMaxCount;
    RecentPathsEnabled := TdxBreadcrumbEditPathEditorProperties(Source).RecentPathsEnabled;
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetAutoComplete(AValue: Boolean);
begin
  if AValue <> FAutoComplete then
  begin
    FAutoComplete := AValue;
    Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetAutoCompleteDropDownRows(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if AValue <> FAutoCompleteDropDownRows then
  begin
    FAutoCompleteDropDownRows := AValue;
    Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    Changed([bcecLayout, bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetPathDelimiter(AValue: Char);
begin
  if AValue <> FPathDelimiter then
  begin
    FPathDelimiter := AValue;
    Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetReadOnly(AValue: Boolean);
begin
  if AValue <> FReadOnly then
  begin
    FReadOnly := AValue;
    if Enabled then
      Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetRecentPaths(AValue: TdxBreadcrumbEditRecentPaths);
begin
  FRecentPaths.Assign(AValue);
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetRecentPathsAutoPopulate(AValue: Boolean);
begin
  if AValue <> FRecentPathsAutoPopulate then
  begin
    FRecentPathsAutoPopulate := AValue;
    Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetRecentPathsDropDownRows(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if AValue <> FRecentPathsDropDownRows then
  begin
    FRecentPathsDropDownRows := AValue;
    Changed([bcecPathEditor]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetRecentPathsEnabled(AValue: Boolean);
begin
  if FRecentPathsEnabled <> AValue then
  begin
    FRecentPathsEnabled := AValue;
    Changed([bcecPathEditor, bcecButtons]);
  end;
end;

procedure TdxBreadcrumbEditPathEditorProperties.SetRecentPathsMaxCount(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FRecentPathsMaxCount then
  begin
    FRecentPathsMaxCount := AValue;
    Changed([bcecPathEditor]);
  end;
end;

{ TdxCustomBreadcrumbEditProperties }

constructor TdxCustomBreadcrumbEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FBorders := cxBordersAll;
  FButtons := CreateButtons;
  FButtons.OnChange := ButtonsChanged;
  FDropDownRows := dxBreadcrumbEditDefaultDropDownRows;
  FDropDownIndent := ddiNone;
  FPathEditor := CreatePathEditorProperties;
  FPathEditor.OnChange := PropertiesChanged;
  FProgressBar := CreateProgressBarProperties;
  FProgressBar.OnChange := PropertiesChanged;
  FButtonImagesChangeLink := TChangeLink.Create;
  FButtonImagesChangeLink.OnChange := ImageListChanged;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChanged;
end;

destructor TdxCustomBreadcrumbEditProperties.Destroy;
begin
  Images := nil;
  ButtonImages := nil;
  FreeAndNil(FButtons);
  FreeAndNil(FPathEditor);
  FreeAndNil(FProgressBar);
  FreeAndNil(FImagesChangeLink);
  FreeAndNil(FButtonImagesChangeLink);
  inherited Destroy;
end;

procedure TdxCustomBreadcrumbEditProperties.Assign(Source: TPersistent);
begin
  if Source is TdxCustomBreadcrumbEditProperties then
  begin
    Borders := TdxCustomBreadcrumbEditProperties(Source).Borders;
    Buttons := TdxCustomBreadcrumbEditProperties(Source).Buttons;
    ButtonImages := TdxCustomBreadcrumbEditProperties(Source).ButtonImages;
    DropDownIndent := TdxCustomBreadcrumbEditProperties(Source).DropDownIndent;
    DropDownRows := TdxCustomBreadcrumbEditProperties(Source).DropDownRows;
    ProgressBar := TdxCustomBreadcrumbEditProperties(Source).ProgressBar;
    Images := TdxCustomBreadcrumbEditProperties(Source).Images;
    PathEditor := TdxCustomBreadcrumbEditProperties(Source).PathEditor;
  end;
end;

procedure TdxCustomBreadcrumbEditProperties.ButtonsChanged(Sender: TObject);
begin
  Changed([bcecButtons, bcecLayout]);
end;

procedure TdxCustomBreadcrumbEditProperties.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Buttons.ChangeScale(M, D);
end;

function TdxCustomBreadcrumbEditProperties.CreateButtons: TdxBreadcrumbEditButtons;
begin
  Result := TdxBreadcrumbEditButtons.Create(Self, TdxBreadcrumbEditButtons.GetButtonClass);
end;

function TdxCustomBreadcrumbEditProperties.CreatePathEditorProperties: TdxBreadcrumbEditPathEditorProperties;
begin
  Result := TdxBreadcrumbEditPathEditorProperties.Create(Self);
end;

function TdxCustomBreadcrumbEditProperties.CreateProgressBarProperties: TdxBreadcrumbEditProgressBarProperties;
begin
  Result := TdxBreadcrumbEditProgressBarProperties.Create(Self);
end;

procedure TdxCustomBreadcrumbEditProperties.InitiateActions;
begin
  Buttons.InitiateActions;
end;

procedure TdxCustomBreadcrumbEditProperties.ImageListChanged(Sender: TObject);
begin
  Changed([bcecLayout]);
end;

procedure TdxCustomBreadcrumbEditProperties.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = ButtonImages then
      ButtonImages := nil;
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TdxCustomBreadcrumbEditProperties.PropertiesChanged(Sender: TObject; AChanges: TdxBreadcrumbEditChanges);
begin
  Changed(AChanges);
end;

procedure TdxCustomBreadcrumbEditProperties.SetBorders(AValue: TcxBorders);
begin
  if AValue <> FBorders then
  begin
    FBorders := AValue;
    Changed([bcecLayout]);
  end;
end;

procedure TdxCustomBreadcrumbEditProperties.SetButtons(AValue: TdxBreadcrumbEditButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TdxCustomBreadcrumbEditProperties.SetButtonImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FButtonImages, FButtonImagesChangeLink, Owner as TComponent);
end;

procedure TdxCustomBreadcrumbEditProperties.SetDropDownIndent(AValue: TdxBreadcrumbEditDropDownIndent);
begin
  if AValue <> FDropDownIndent then
  begin
    FDropDownIndent := AValue;
    Changed([bcecLayout]);
  end;
end;

procedure TdxCustomBreadcrumbEditProperties.SetDropDownRows(AValue: Integer);
begin
  FDropDownRows := Max(AValue, 1);
end;

procedure TdxCustomBreadcrumbEditProperties.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FImagesChangeLink, Owner as TComponent);
end;

procedure TdxCustomBreadcrumbEditProperties.SetPathEditor(AValue: TdxBreadcrumbEditPathEditorProperties);
begin
  FPathEditor.Assign(AValue)
end;

procedure TdxCustomBreadcrumbEditProperties.SetProgressBar(AValue: TdxBreadcrumbEditProgressBarProperties);
begin
  FProgressBar.Assign(AValue);
end;

{ TdxBreadcrumbEditHitTestInfo }

procedure TdxBreadcrumbEditHitTestInfo.Reset;
begin
  Point := cxNullPoint;
  ViewItemPart := nvipNone;
  ViewItem := nil;
end;

{ TdxBreadcrumbEditPartCustomViewInfo }

constructor TdxBreadcrumbEditPartCustomViewInfo.Create(AOwner: TdxBreadcrumbEditViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  FViewItemList := TcxObjectList.Create;
end;

destructor TdxBreadcrumbEditPartCustomViewInfo.Destroy;
begin
  FViewItemList.Clear;
  FreeAndNil(FViewItemList);
  inherited Destroy;
end;

function TdxBreadcrumbEditPartCustomViewInfo.CalculateAutoHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ViewItemCount - 1 do
    Result := Max(Result, ViewItems[I].CalculateAutoHeight);
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.CalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo);
var
  I: Integer;
begin
  for I := 0 to ViewItemCount - 1 do
  begin
    ViewItems[I].CalculateHitTest(AInfo);
    if AInfo.ViewItem <> nil then
      Break;
  end;
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if not IsRectEmpty(Bounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(Bounds);
      InternalDraw(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.InternalDraw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to ViewItemCount - 1 do
    ViewItems[I].Draw(ACanvas);
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.Invalidate;
begin
  Invalidate(Bounds);
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.Invalidate(const R: TRect);
begin
  Owner.Invalidate(R);
end;

function TdxBreadcrumbEditPartCustomViewInfo.PlaceAt(var R: TRect; AWidth: Integer; AAtRightSide: Boolean): TRect;
begin
  if AAtRightSide then
  begin
    Result := cxRectSetLeft(R, R.Left, AWidth);
    R.Left := Result.Right;
  end
  else
  begin
    Result := cxRectSetRight(R, R.Right, AWidth);
    R.Right := Result.Left;
  end;
end;

function TdxBreadcrumbEditPartCustomViewInfo.IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean;
begin
  Result := FViewItemList.IndexOf(AItem) >= 0;
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.RecreateViewItems;
begin
end;

procedure TdxBreadcrumbEditPartCustomViewInfo.UpdateViewItemsState(AInfo: TdxBreadcrumbEditHitTestInfo);
var
  I: Integer;
begin
  for I := 0 to ViewItemCount - 1 do
    ViewItems[I].UpdateStates(AInfo);
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetControl: IdxBreadcrumbEdit;
begin
  Result := Owner.Control;
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.GetPainter;
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := Owner.GetProperties;
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.GetScaleFactor;
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetViewItem(Index: Integer): TdxBreadcrumbEditViewItem;
begin
  Result := TdxBreadcrumbEditViewItem(FViewItemList[Index]);
end;

function TdxBreadcrumbEditPartCustomViewInfo.GetViewItemCount: Integer;
begin
  Result := FViewItemList.Count;
end;

{ TdxBreadcrumbEditButtonsPartViewInfo }

procedure TdxBreadcrumbEditButtonsPartViewInfo.Calculate(R: TRect);

  function GetButtonWidth(AButtonViewItem: TdxBreadcrumbEditButtonViewItem): Integer;
  begin
    if AButtonViewItem.Button.Width > 0 then
      Result := AButtonViewItem.Button.Width
    else
      Result := AButtonViewItem.CalculateAutoWidth;
  end;

var
  AButtonViewItem: TdxBreadcrumbEditButtonViewItem;
  AUseRTLAlignment: Boolean;
  I: Integer;
begin
  FBounds := R;

  AUseRTLAlignment := UseRightToLeftAlignment;
  for I := ViewItemCount - 1 downto 0 do
  begin
    AButtonViewItem := TdxBreadcrumbEditButtonViewItem(ViewItems[I]);
    AButtonViewItem.Calculate(PlaceAt(R, GetButtonWidth(AButtonViewItem), AUseRTLAlignment));
  end;

  if ViewItemCount > 0 then
    FSeparatorRect := PlaceAt(R, Painter.BreadcrumbEditScaledButtonAreaSeparatorSize(ScaleFactor), AUseRTLAlignment)
  else
    FSeparatorRect := cxNullRect;

  if UseRightToLeftAlignment then
    FBounds.Right := R.Left
  else
    FBounds.Left := R.Right;
end;

function TdxBreadcrumbEditButtonsPartViewInfo.CreateViewItem(
  AButton: TdxBreadcrumbEditButton): TdxBreadcrumbEditButtonViewItem;
begin
  Result := TdxBreadcrumbEditButtonViewItem.Create(Self, AButton);
end;

procedure TdxBreadcrumbEditButtonsPartViewInfo.InternalDraw(ACanvas: TcxCanvas);
begin
  inherited InternalDraw(ACanvas);
  Painter.DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas, SeparatorRect, Owner.State, ScaleFactor);
end;

procedure TdxBreadcrumbEditButtonsPartViewInfo.RecreateViewItems;
var
  I: Integer;
begin
  FViewItemList.Clear;
  for I := 0 to Properties.Buttons.Count - 1 do
  begin
    if Properties.Buttons[I].Visible then
      FViewItemList.Add(CreateViewItem(Properties.Buttons[I]));
  end;
end;

{ TdxBreadcrumbEditDropDownButtonViewItem }

function TdxBreadcrumbEditDropDownButtonViewItem.CalculateAutoHeight: Integer;
begin
  Result := ScaleFactor.Apply(dxBreadcrumbEditDefaultImageSize) +
    cxMarginsHeight(Painter.BreadcrumbEditScaledButtonContentOffsets(True, True, ScaleFactor));
end;

procedure TdxBreadcrumbEditDropDownButtonViewItem.Click(APart: Integer = nvipControl);
begin
  if APart = nvipControl then
    Controller.PathEditingController.DropDown;
end;

procedure TdxBreadcrumbEditDropDownButtonViewItem.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawBreadcrumbEditScaledDropDownButton(ACanvas, Bounds, State, False, ScaleFactor);
end;

procedure TdxBreadcrumbEditDropDownButtonViewItem.DrawContent(ACanvas: TcxCanvas);
begin
  Painter.DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas, Bounds, State, False, ScaleFactor);
end;

{ TdxBreadcrumbEditNodesAreaPartViewInfo }

constructor TdxBreadcrumbEditNodesAreaPartViewInfo.Create(AOwner: TdxBreadcrumbEditViewInfo);
begin
  inherited Create(AOwner);
  FDropDownButton := CreateDropDownButton;
end;

destructor TdxBreadcrumbEditNodesAreaPartViewInfo.Destroy;
begin
  FreeAndNil(FDropDownButton);
  inherited Destroy;
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.Calculate(R: TRect);
begin
  FBounds := R;
  CalculateImageRect(R);
  CalculateDropDownButton(R);
  CalculateNodesWidths(cxRectWidth(R));
  CalculateNodesBounds(R);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateAutoHeight: Integer;
begin
  Result := Max(inherited CalculateAutoHeight, DropDownButton.CalculateAutoHeight);
  Result := Max(Result, GetDefaultImageHeight + 2 * dxBreadcrumbEditImageBorder);
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateDropDownButton(var R: TRect);
var
  AWidth: Integer;
begin
  if Properties.PathEditor.Enabled and Properties.PathEditor.RecentPathsEnabled then
    AWidth := GetDropDownButtonWidth
  else
    AWidth := 0;

  DropDownButton.Calculate(PlaceAt(R, AWidth, UseRightToLeftAlignment));
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo);
begin
  DropDownButton.CalculateHitTest(AInfo);
  if AInfo.ViewItem = nil then
    inherited CalculateHitTest(AInfo);
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateImageRect(var R: TRect);
var
  AImageSize: TSize;
begin
  if Properties.Images <> nil then
  begin
    AImageSize := dxGetImageSize(Properties.Images, ScaleFactor);
    FImageRect := cxRectCenter(PlaceAt(R, AImageSize.cx + 2 * dxBreadcrumbEditImageBorder, not UseRightToLeftAlignment), AImageSize);
  end
  else
    FImageRect := cxNullRect;
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateNodesBounds(R: TRect);
var
  ANodeViewItem: TdxBreadcrumbEditNodeViewItem;
  AUseRightToLeftAlignment: Boolean;
  I: Integer;
begin
  AUseRightToLeftAlignment := UseRightToLeftAlignment;
  for I := 0 to ViewItemCount - 1 do
  begin
    ANodeViewItem := ViewItems[I];
    ANodeViewItem.Calculate(PlaceAt(R, ANodeViewItem.Width, not AUseRightToLeftAlignment));
  end;
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.CalculateNodesWidths(AContentWidth: Integer);

  function GetContentWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to ViewItemCount - 1 do
      Inc(Result, ViewItems[I].Width);
  end;

  procedure HideNodeViewItem(ANodeViewItem: TdxBreadcrumbEditNodeViewItem);
  begin
    if ANodeViewItem.CanHideNode then
      ANodeViewItem.FWidth := 0;
  end;

  function ReduceNodeViewItemWidth(ANodeViewItem: TdxBreadcrumbEditNodeViewItem): Boolean;
  begin
    Result := ANodeViewItem.Width > ANodeViewItem.MinWidth;
    if Result then
      Dec(ANodeViewItem.FWidth);
  end;

var
  AIndex: Integer;
begin
  for AIndex := 0 to ViewItemCount - 1 do
    ViewItems[AIndex].CalculateSizes;

  AIndex := 0;
  while (GetContentWidth > AContentWidth) and (AIndex + dxBreadcrumbEditMinVisibleNodesCount < ViewItemCount) do
  begin
    HideNodeViewItem(ViewItems[AIndex]);
    Inc(AIndex);
  end;
  while (GetContentWidth > AContentWidth) and (AIndex < ViewItemCount) do
  begin
    if not ReduceNodeViewItemWidth(ViewItems[AIndex]) then
      Inc(AIndex);
  end;
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.CreateDropDownButton: TdxBreadcrumbEditViewItem;
begin
  Result := TdxBreadcrumbEditDropDownButtonViewItem.Create(Self);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.CreateViewItem(
  ANode: TdxBreadcrumbEditNode): TdxBreadcrumbEditNodeViewItem;
begin
  if ANode.IsRoot then
    Result := TdxBreadcrumbEditRootNodeViewItem.Create(Self, ANode)
  else
    Result := TdxBreadcrumbEditNodeViewItem.Create(Self, ANode);
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.DrawImage(
  ACanvas: TcxCanvas; const ARect: TRect; AImageIndex: TcxImageIndex);
begin
  if IsImageAssigned(Properties.Images, AImageIndex) then
    cxDrawImage(ACanvas.Handle, ARect, ARect, nil, Properties.Images,
      AImageIndex, EnabledImageDrawModeMap[Owner.State <> dxbcsDisabled]);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.FindViewItem(
  ANode: TdxBreadcrumbEditNode; out AViewItem: TdxBreadcrumbEditNodeViewItem): Boolean;
var
  I: Integer;
begin
  AViewItem := nil;
  for I := 0 to ViewItemCount - 1 do
    if ViewItems[I].Node = ANode then
    begin
      AViewItem := ViewItems[I];
      Break;
    end;

  Result := AViewItem <> nil;
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.InternalDraw(ACanvas: TcxCanvas);
begin
  if not IsRectEmpty(ImageRect) and (SelectedNodeViewItem <> nil) then
  begin
    DrawImage(ACanvas, ImageRect, SelectedNodeViewItem.Node.ImageIndex);
    ACanvas.ExcludeClipRect(ImageRect);
  end;
  if DropDownButton.Visible then
  begin
    DropDownButton.Draw(ACanvas);
    ACanvas.ExcludeClipRect(DropDownButton.Bounds);
  end;
  inherited InternalDraw(ACanvas);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean;
begin
  Result := inherited IsViewItemValid(AItem) or (DropDownButton = AItem);
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.RecreateViewItems;

  function CreateViewItemsList(ANode: TdxBreadcrumbEditNode): TList;
  var
    AViewItem: TdxBreadcrumbEditNodeViewItem;
  begin
    Result := TList.Create;
    while ANode <> nil do
    begin
      if FindViewItem(ANode, AViewItem) then
        FViewItemList.Remove(AViewItem)
      else
        AViewItem := CreateViewItem(ANode);

      Result.Insert(0, AViewItem);
      ANode := ANode.Parent;
    end;
  end;

var
  ATempViewItemsList: TList;
begin
  ATempViewItemsList := CreateViewItemsList(Control.GetController.Selected);
  try
    FViewItemList.Assign(ATempViewItemsList);
  finally
    ATempViewItemsList.Free;
  end;
end;

procedure TdxBreadcrumbEditNodesAreaPartViewInfo.UpdateViewItemsState(AInfo: TdxBreadcrumbEditHitTestInfo);
begin
  inherited UpdateViewItemsState(AInfo);
  DropDownButton.UpdateStates(AInfo);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetDropDownButtonWidth: Integer;
begin
  Result := Painter.BreadcrumbEditScaledDropDownButtonWidth(ScaleFactor);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetFirstViewItem: TdxBreadcrumbEditNodeViewItem;
begin
  if ViewItemCount > 0 then
    Result := ViewItems[0]
  else
    Result := nil;
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetDefaultImageHeight: Integer;
begin
  if Properties.Images <> nil then
    Result := dxGetImageSize(Properties.Images, ScaleFactor).cy
  else
    Result := ScaleFactor.Apply(dxBreadcrumbEditDefaultImageSize);
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetPathEditorRect: TRect;
begin
  Result := Bounds;
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetSelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem;
begin
  if ViewItemCount > 0 then
    Result := ViewItems[ViewItemCount - 1]
  else
    Result := nil;
end;

function TdxBreadcrumbEditNodesAreaPartViewInfo.GetViewItem(Index: Integer): TdxBreadcrumbEditNodeViewItem;
begin
  Result := TdxBreadcrumbEditNodeViewItem(FViewItemList[Index]);
end;

{ TdxBreadcrumbEditProgressBarPartViewInfo }

procedure TdxBreadcrumbEditProgressBarPartViewInfo.Calculate(R: TRect);
begin
  FBounds := cxRectContent(R, Painter.BreadcrumbEditScaledProgressChunkPadding(ScaleFactor));
  FOverlaySize := Painter.BreadcrumbEditScaledProgressChunkOverlaySize(ScaleFactor);
  ResetOverlayPosition;
end;

procedure TdxBreadcrumbEditProgressBarPartViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  AChunkRect: TRect;
begin
  AChunkRect := ChunkRect;
  if not IsRectEmpty(AChunkRect) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(AChunkRect);
      Painter.DrawBreadcrumbEditScaledProgressChunk(ACanvas, AChunkRect, ScaleFactor);
      Painter.DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas, OverlayRect, ScaleFactor);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxBreadcrumbEditProgressBarPartViewInfo.InternalDraw(ACanvas: TcxCanvas);
var
  ABitmap: TcxBitmap32;
begin
  if Opacity = MaxByte then
    DrawContent(ACanvas)
  else
    if Opacity > 0 then
    begin
      ABitmap := TcxBitmap32.CreateSize(Bounds, False);
      try
        cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
        ABitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
        DrawContent(ABitmap.cxCanvas);
        ABitmap.cxCanvas.WindowOrg := cxNullPoint;
        ABitmap.MakeOpaque;
        cxAlphaBlend(ACanvas.Handle, ABitmap, Bounds, ABitmap.ClientRect, False, Opacity);
      finally
        ABitmap.Free;
      end;
    end;
end;

procedure TdxBreadcrumbEditProgressBarPartViewInfo.ResetOverlayPosition;
begin
  OverlayPosition := -OverlaySize.cx;
end;

function TdxBreadcrumbEditProgressBarPartViewInfo.GetChunkRect: TRect;
begin
  Result := Bounds;
  Result.Right := Result.Left + Trunc(cxRectWidth(Result) * Progress / 100);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TdxBreadcrumbEditProgressBarPartViewInfo.GetOverlayRect: TRect;
begin
  Result := cxRectSetLeft(Bounds, OverlayPosition, OverlaySize.cx);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

procedure TdxBreadcrumbEditProgressBarPartViewInfo.SetProgress(AValue: Single);
begin
  FProgress := ValidateProgressValue(AValue);
end;

{ TdxBreadcrumbEditViewInfo }

constructor TdxBreadcrumbEditViewInfo.Create(const AControl: IdxBreadcrumbEdit);
begin
  inherited Create;
  FControl := AControl;
  FHitTestInfo := CreateHitTestInfo;
  FButtonsViewInfo := CreateButtonsViewInfo;
  FProgressBarViewInfo := CreateProgressBarViewInfo;
  FNodesAreaViewInfo := CreateNodesAreaViewInfo;
end;

destructor TdxBreadcrumbEditViewInfo.Destroy;
begin
  FControl := nil;
  FreeAndNil(FNodesAreaViewInfo);
  FreeAndNil(FProgressBarViewInfo);
  FreeAndNil(FButtonsViewInfo);
  FreeAndNil(FHitTestInfo);
  inherited Destroy;
end;

procedure TdxBreadcrumbEditViewInfo.Calculate(R: TRect);
begin
  FBounds := R;
  R := ContentRect;
  ProgressBarViewInfo.Calculate(R);
  ButtonsViewInfo.Calculate(R);

  if UseRightToLeftAlignment then
    R.Left := ButtonsViewInfo.Bounds.Right
  else
    R.Right := ButtonsViewInfo.Bounds.Left;

  NodesAreaViewInfo.Calculate(R);
end;

function TdxBreadcrumbEditViewInfo.CalculateAutoHeight: Integer;
begin
  Result := Max(0, NodesAreaViewInfo.CalculateAutoHeight);
  Result := Max(Result, ButtonsViewInfo.CalculateAutoHeight);
  Result := Max(Result, ProgressBarViewInfo.CalculateAutoHeight);
  Result := Max(Result, dxBreadcrumbEditDefaultHeight);
  Inc(Result, cxMarginsHeight(BordersWidth));
end;

procedure TdxBreadcrumbEditViewInfo.CalculateHitTest(const P: TPoint);
begin
  HitTestInfo.Reset;
  HitTestInfo.Point := P;
  DoCalculateHitTest(HitTestInfo);
end;

procedure TdxBreadcrumbEditViewInfo.CalculateHitTest(X, Y: Integer);
begin
  CalculateHitTest(Point(X, Y));
end;

function TdxBreadcrumbEditViewInfo.CreateButtonsViewInfo: TdxBreadcrumbEditButtonsPartViewInfo;
begin
  Result := TdxBreadcrumbEditButtonsPartViewInfo.Create(Self);
end;

function TdxBreadcrumbEditViewInfo.CreateHitTestInfo: TdxBreadcrumbEditHitTestInfo;
begin
  Result := TdxBreadcrumbEditHitTestInfo.Create;
end;

function TdxBreadcrumbEditViewInfo.CreateNodesAreaViewInfo: TdxBreadcrumbEditNodesAreaPartViewInfo;
begin
  Result := TdxBreadcrumbEditNodesAreaPartViewInfo.Create(Self);
end;

function TdxBreadcrumbEditViewInfo.CreateProgressBarViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo;
begin
  Result := TdxBreadcrumbEditProgressBarPartViewInfo.Create(Self);
end;

procedure TdxBreadcrumbEditViewInfo.DoCalculateHitTest(AInfo: TdxBreadcrumbEditHitTestInfo);
begin
  ButtonsViewInfo.CalculateHitTest(AInfo);
  if AInfo.ViewItem = nil then
    NodesAreaViewInfo.CalculateHitTest(AInfo);
  if AInfo.ViewItem = nil then
    ProgressBarViewInfo.CalculateHitTest(AInfo);
end;

procedure TdxBreadcrumbEditViewInfo.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.SaveClipRegion;
  try
    Painter.DrawBreadcrumbEditBorders(ACanvas, Bounds, Properties.Borders, State);
    ACanvas.IntersectClipRect(ContentRect);
    ProgressBarViewInfo.Draw(ACanvas);
    ButtonsViewInfo.Draw(ACanvas);
    NodesAreaViewInfo.Draw(ACanvas);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxBreadcrumbEditViewInfo.GetNavigationOrder(AList: TList);
var
  I: Integer;
begin
  for I := 0 to NodesAreaViewInfo.ViewItemCount - 1 do
  begin
    if NodesAreaViewInfo.ViewItems[I].Visible then
      AList.Add(NodesAreaViewInfo.ViewItems[I]);
  end;
end;

procedure TdxBreadcrumbEditViewInfo.Invalidate;
begin
  Invalidate(Bounds);
end;

procedure TdxBreadcrumbEditViewInfo.Invalidate(const R: TRect);
begin
  if Control.GetContainer.HandleAllocated then
    cxInvalidateRect(Control.GetContainer.Handle, R);
end;

function TdxBreadcrumbEditViewInfo.IsViewItemValid(AItem: TdxBreadcrumbEditViewItem): Boolean;
begin
  Result := ButtonsViewInfo.IsViewItemValid(AItem) or NodesAreaViewInfo.IsViewItemValid(AItem);
end;

procedure TdxBreadcrumbEditViewInfo.UpdateViewItemStates;
begin
  ButtonsViewInfo.UpdateViewItemsState(HitTestInfo);
  ProgressBarViewInfo.UpdateViewItemsState(HitTestInfo);
  NodesAreaViewInfo.UpdateViewItemsState(HitTestInfo);
end;

function TdxBreadcrumbEditViewInfo.GetBordersWidth: TRect;
begin
  Result := Painter.BreadcrumbEditBordersSize;
  if not (bBottom in Properties.Borders) then
    Result.Bottom := 0;
  if not (bRight in Properties.Borders) then
    Result.Right := 0;
  if not (bLeft in Properties.Borders) then
    Result.Left := 0;
  if not (bTop in Properties.Borders) then
    Result.Top := 0;
end;

function TdxBreadcrumbEditViewInfo.GetContentRect: TRect;
begin
  Result := cxRectContent(Bounds, BordersWidth);
end;

function TdxBreadcrumbEditViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Control.GetLookAndFeel.Painter;
end;

function TdxBreadcrumbEditViewInfo.GetProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := Control.GetProperties;
end;

function TdxBreadcrumbEditViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

procedure TdxBreadcrumbEditViewInfo.SetState(AState: TdxBreadcrumbEditState);
begin
  if AState <> FState then
  begin
    FState := AState;
    Invalidate;
  end;
end;

{ TdxBreadcrumbEditProgressBarController }

constructor TdxBreadcrumbEditProgressBarController.Create(AController: TdxBreadcrumbEditController);
begin
  inherited Create;
  FController := AController;
end;

destructor TdxBreadcrumbEditProgressBarController.Destroy;
begin
  StopFading;
  StopCancelEffectAnimation;
  StopOverlayAnimation;
  inherited Destroy;
end;

procedure TdxBreadcrumbEditProgressBarController.Fade(AFramesCount: Integer; AFadeOut: Boolean);
const
  StatesMap: array[Boolean] of TdxBreadcrumbEditProgressBarFadingState = (bcpfsFadeIn, bcpfsFadeOut);
begin
  if FadingState = bcpfsNone then
  begin
    FFadingState := StatesMap[AFadeOut];
    if dxFader.IsReady and ViewInfo.Painter.BreadcrumbEditIsFadingSupports and (AFramesCount > 0) then
    begin
      FFadingFrameIndex := 0;
      FFadingFramesCount := AFramesCount;
      if FFadingTimer = nil then
      begin
        FFadingTimer := TcxTimer.Create(nil);
        FFadingTimer.OnTimer := ProcessFadeStep;
        FFadingTimer.Interval := 30;
        FFadingTimer.Enabled := True;
      end;
      ViewInfo.Opacity := IfThen(AFadeOut, MaxByte);
      UpdateOverlayAnimationState;
    end
    else
      StopFading;
  end;
end;

procedure TdxBreadcrumbEditProgressBarController.FadeIn;
begin
  Fade(dxBreadcrumbEditProgressFadeInFramesCount, False);
end;

procedure TdxBreadcrumbEditProgressBarController.FadeOut;
begin
  Fade(dxBreadcrumbEditProgressFadeOutFramesCount, True);
end;

procedure TdxBreadcrumbEditProgressBarController.ProcessCancelEffectAnimationStep(Sender: TObject);

  function GetIsCancelEffectAnimationDone: Boolean;
  begin
    case Properties.CancelEffect of
      bpceFillAreaForward:
        Result := SameValue(ViewInfo.Progress, 100);
      bpceFillAreaBackward:
        Result := SameValue(ViewInfo.Progress, 0);
      else
        Result := True;
    end;
  end;

const
  Direction: array[TdxBreadcrumbEditProgressBarCancelEffect] of Single = (0, 1, -1);
begin
  if GetIsCancelEffectAnimationDone then
  begin
    FadeOut;
    StopCancelEffectAnimation;
  end
  else
  begin
    ViewInfo.Progress := ViewInfo.Progress +
      Direction[Properties.CancelEffect] * dxBreadcrumbEditProgressFillingDelta;
    ViewInfo.Invalidate;
  end;
end;

procedure TdxBreadcrumbEditProgressBarController.ProcessFadeStep(Sender: TObject);
begin
  Inc(FFadingFrameIndex);
  if FFadingFrameIndex <= FFadingFramesCount then
  begin
    ViewInfo.Opacity := FadingStageAlpha;
    ViewInfo.Invalidate;
  end
  else
    StopFading;
end;

procedure TdxBreadcrumbEditProgressBarController.ProcessOverlayAnimationStep(Sender: TObject);
begin
  if FOverlayAnimationRestartDelayCount = 0 then
  begin
    ViewInfo.OverlayPosition := ViewInfo.OverlayPosition + OverlayAnimationOffset;
    if not cxRectIntersect(ViewInfo.OverlayRect, ViewInfo.ChunkRect) then
    begin
      FOverlayAnimationRestartDelayCount := Round(
        Properties.AnimationRestartDelay / OverlayAnimationTimer.Interval);
      ViewInfo.ResetOverlayPosition;
    end;
    ViewInfo.Invalidate;
  end
  else
    Dec(FOverlayAnimationRestartDelayCount);
end;

procedure TdxBreadcrumbEditProgressBarController.StartCancelEffectAnimation;
begin
  if FCancelEffectAnimationTimer = nil then
  begin
    FCancelEffectAnimationTimer := TcxTimer.Create(nil);
    FCancelEffectAnimationTimer.OnTimer := ProcessCancelEffectAnimationStep;
    FCancelEffectAnimationTimer.Interval := 30;
    FCancelEffectAnimationTimer.Enabled := True;
  end;
  StopOverlayAnimation;
  StopFading;
end;

procedure TdxBreadcrumbEditProgressBarController.StartOverlayAnimation;
begin
  if OverlayAnimationTimer = nil then
  begin
    ViewInfo.ResetOverlayPosition;
    FOverlayAnimationRestartDelayCount := 0;
    FOverlayAnimationTimer := TcxTimer.Create(nil);
    FOverlayAnimationTimer.OnTimer := ProcessOverlayAnimationStep;
    FOverlayAnimationTimer.Enabled := True;
  end;
  OverlayAnimationTimer.Interval := OverlayAnimationTimerInterval;
end;

procedure TdxBreadcrumbEditProgressBarController.StopCancelEffectAnimation;
begin
  FreeAndNil(FCancelEffectAnimationTimer);
end;

procedure TdxBreadcrumbEditProgressBarController.StopFading;
begin
  if FadingState <> bcpfsNone then
  begin
    FreeAndNil(FFadingTimer);
    ViewInfo.Opacity := IfThen(FadingState = bcpfsFadeIn, MaxByte);
    FFadingState := bcpfsNone;
    UpdateOverlayAnimationState;
    ViewInfo.Invalidate;
  end;
end;

procedure TdxBreadcrumbEditProgressBarController.StopOverlayAnimation;
begin
  if OverlayAnimationTimer <> nil then
  begin
    FreeAndNil(FOverlayAnimationTimer);
    ViewInfo.ResetOverlayPosition;
    ViewInfo.Invalidate;
  end;
end;

procedure TdxBreadcrumbEditProgressBarController.UpdateOverlayAnimationState;
begin
  if OverlayAnimationEnabled and (Progress > 0) then
    StartOverlayAnimation
  else
    StopOverlayAnimation;
end;

procedure TdxBreadcrumbEditProgressBarController.UpdateProgress;
begin
  if Properties.MaxValue > Properties.MinValue then
    Progress := 100 * (Properties.Position - Properties.MinValue) / (Properties.MaxValue - Properties.MinValue)
  else
    Progress := 0;
end;

function TdxBreadcrumbEditProgressBarController.GetCancelEffectAnimationActive: Boolean;
begin
  Result := CancelEffectAnimationTimer <> nil;
end;

function TdxBreadcrumbEditProgressBarController.GetFadingStageAlpha: Byte;
begin
  Result := MulDiv(MaxByte, FFadingFrameIndex, FFadingFramesCount);
  if FadingState = bcpfsFadeOut then
    Result := MaxByte - Result;
end;

function TdxBreadcrumbEditProgressBarController.GetIsOverlayAnimationEnabled: Boolean;
begin
  Result := Properties.Animation and (FadingState = bcpfsNone) and
    not CancelEffectAnimationActive and (ViewInfo.OverlaySize.cx > 0);
end;

function TdxBreadcrumbEditProgressBarController.GetOverlayAnimationOffset: Integer;
var
  AAnimationSpeed: Cardinal;
begin
  AAnimationSpeed := Properties.AnimationSpeed;
  if AAnimationSpeed >= High(AAnimationSpeed) div 2 then
    Result := 2
  else
    Result := 2 + (AAnimationSpeed + High(AAnimationSpeed)) * 2;
end;

function TdxBreadcrumbEditProgressBarController.GetOverlayAnimationTimerInterval: Cardinal;
var
  AAnimationSpeed: Cardinal;
begin
  AAnimationSpeed := Properties.AnimationSpeed;
  if AAnimationSpeed <= High(AAnimationSpeed) div 2 then
    Result := 30
  else
    Result := 30 + (High(AAnimationSpeed) - AAnimationSpeed) * 4;
end;

function TdxBreadcrumbEditProgressBarController.GetProperties: TdxBreadcrumbEditProgressBarProperties;
begin
  Result := Controller.ViewInfo.Properties.ProgressBar;
end;

function TdxBreadcrumbEditProgressBarController.GetViewInfo: TdxBreadcrumbEditProgressBarPartViewInfo;
begin
  Result := Controller.ViewInfo.ProgressBarViewInfo;
end;

procedure TdxBreadcrumbEditProgressBarController.SetProgress(AValue: Single);

  function IsAnimationOnResetValueActive: Boolean;
  begin
    Result := (FadingState = bcpfsFadeOut) or CancelEffectAnimationActive;
  end;

  function CanStartFadeInAnimation: Boolean;
  begin
    Result := (FadingState = bcpfsNone) and not CancelEffectAnimationActive and (ViewInfo.Opacity = 0);
  end;

  function IsProgressCancelled: Boolean;
  begin
    Result := not SameValue(Progress, 100);
  end;

  procedure CheckAnimationState(ANewProgressValue: Single);
  begin
    if not IsZero(ANewProgressValue) then
    begin
      if CanStartFadeInAnimation then
        FadeIn
      else
        UpdateOverlayAnimationState;
    end
    else
      if not IsAnimationOnResetValueActive then
      begin
        if IsProgressCancelled then
          StartCancelEffectAnimation
        else
          FadeOut;
      end;
  end;

begin
  AValue := ValidateProgressValue(AValue);
  if AValue <> FProgress then
  begin
    CheckAnimationState(AValue);
    FProgress := AValue;
    if not IsAnimationOnResetValueActive then
      ViewInfo.Progress := Progress;
    if FadingState = bcpfsNone then
      UpdateOverlayAnimationState;
    ViewInfo.Invalidate;
  end;
end;

{ TdxBreadcrumbEditNodeDropDownMenu }

constructor TdxBreadcrumbEditNodeDropDownMenu.Create(AController: TdxBreadcrumbEditController);
begin
  inherited Create(AController.ControlContainer);
  FController := AController;
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.Popup;
var
  P: TPoint;
  ASize: TSize;
begin
  if IsVisible then
  begin
    InitPopup;
    ASize := CalculateSize;
    P := CalculatePosition(ASize);
    CorrectBoundsWithDesktopWorkArea(P, ASize);
    SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
  end
  else
    inherited Popup;
end;

function TdxBreadcrumbEditNodeDropDownMenu.CreateInnerListBox: TdxCustomDropDownInnerListBox;
begin
  Result := TdxBreadcrumbEditDropDownInnerListBox.Create(nil);
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.DoCloseUp(AClosedViaKeyboard: Boolean);
begin
  PostMessage(Controller.Handle, DXM_BREADCRUMBEDIT_CLOSEPOPUPWINDOW, Ord(AClosedViaKeyboard), 0);
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.DoSelectItem(
  AItem: TdxCustomListBoxItem; ASelectedViaKeyboard: Boolean);
begin
  Controller.SelectPathViaDropDownMenu(AItem.Data as TdxBreadcrumbEditNode);
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.InitInnerListBox;

  function GetNode(AItem: TdxCustomListBoxItem): TdxBreadcrumbEditNode;
  begin
    Result := AItem.Data as TdxBreadcrumbEditNode;
  end;

  procedure CheckHasSeparator(AItem: TdxCustomListBoxItem; AIsLast: Boolean);
  begin
    AItem.HasSeparator := not AIsLast and GetNode(AItem).IsRoot;
  end;

  procedure AddSeparators;
  var
    I: Integer;
  begin
    for I := 0 to InnerListBox.Items.Count - 1 do
      CheckHasSeparator(InnerListBox.Items[I], I + 1 = InnerListBox.Count);
  end;

  function GetDefaultItemIndex(ASelectedNode: TdxBreadcrumbEditNode): Integer;
  var
    ANode: TdxBreadcrumbEditNode;
    I: Integer;
  begin
    Result := -1;
    if ASelectedNode <> nil then
      for I := InnerListBox.Items.Count - 1 downto 0 do
      begin
        ANode := GetNode(InnerListBox.Items[I]);
        if (ANode = ASelectedNode) or ASelectedNode.HasAsParent(ANode) then
        begin
          Result := I;
          Break;
        end;
      end;
  end;

begin
  inherited InitInnerListBox;
  InnerListBox.LoopedNavigation := True;
  InnerListBox.LookAndFeel.MasterLookAndFeel := Controller.Control.GetLookAndFeel;
  InnerListBox.Images := Controller.Control.GetProperties.Images;
  InnerListBox.Font := Controller.Control.GetFont;

  InnerListBox.BeginUpdate;
  try
    InnerListBox.Clear;
    InnerListBox.ItemIndex := -1;
    Controller.DropDownMenuOwner.BuildDropDownMenu(InnerListBox.Items);
    AddSeparators;
    InnerListBox.UpdateRoot;
  finally
    InnerListBox.EndUpdate;
    InnerListBox.DefaultItemIndex := GetDefaultItemIndex(Controller.Selected);
  end;
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.InitPopup;
begin
  inherited InitPopup;
  MaxWidth := dxBreadcrumbEditMenuMaxWidth;
  MinWidth := dxBreadcrumbEditMenuMinWidth;
  OwnerParent := Controller.ControlContainer;
  OwnerBounds := Controller.DropDownMenuOwner.Bounds;
  DisplayRowsCount := Controller.Control.GetProperties.DropDownRows;
  BiDiMode := Controller.ControlContainer.BiDiMode;
  if UseRightToLeftAlignment then
    AlignHorz := pahRight;
end;

procedure TdxBreadcrumbEditNodeDropDownMenu.InternalCalculateVerticalDirectionPosition(
  var APosition: TPoint; var AOrigin: TPoint; const ASize: TSize);
var
  AOffset: Integer;
begin
  if (Controller.Control.GetProperties.DropDownIndent = ddiExplorerLike) and (Controller.DropDownMenuOwner.Index > 0) then
  begin
    AOffset := Controller.DropDownMenuOwner.GetDelimiterSize + ScaleFactor.Apply(dxBreadcrumbEditDefaultDropDownIndentValue);
    if UseRightToLeftAlignment then
    begin
      APosition.X := OwnerScreenBounds.Right - AOffset - ASize.cx;
      AOrigin.X := OwnerScreenBounds.Right - 1;
    end
    else
    begin
      APosition.X := OwnerScreenBounds.Right - AOffset;
      AOrigin.X := OwnerScreenBounds.Left;
    end;

    case AlignVert of
      pavTop:
        begin
          APosition.Y := OwnerScreenBounds.Top - ASize.cy;
          AOrigin.Y := OwnerScreenBounds.Top;
        end;
      pavBottom:
        begin
          APosition.Y := OwnerScreenBounds.Bottom;
          AOrigin.Y := OwnerScreenBounds.Bottom;
        end;
    end;
  end
  else
    inherited InternalCalculateVerticalDirectionPosition(APosition, AOrigin, ASize);
end;

function TdxBreadcrumbEditNodeDropDownMenu.GetInnerListBox: TdxBreadcrumbEditDropDownInnerListBox;
begin
  Result := TdxBreadcrumbEditDropDownInnerListBox(inherited InnerListBox);
end;

function TdxBreadcrumbEditNodeDropDownMenu.GetRootAlwaysVisible: Boolean;
begin
  Result := Controller.DropDownMenuOwner.Node = Controller.Root;
end;

{ TdxBreadcrumbEditDropDownInnerListBox }

constructor TdxBreadcrumbEditDropDownInnerListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRootItemSize := cxNullSize;
  FRootItem := TdxCustomListBoxItem.Create(nil);
  FRootItemState := cxbsNormal;
end;

destructor TdxBreadcrumbEditDropDownInnerListBox.Destroy;
begin
  FreeAndNil(FRootItem);
  inherited Destroy;
end;

function TdxBreadcrumbEditDropDownInnerListBox.CalculateContentSize(AMaxVisibleItemsCount: Integer): TSize;
begin
  Result := inherited CalculateContentSize(AMaxVisibleItemsCount);
  Result.cx := Max(Result.cx, FRootItemSize.cx);
  Inc(Result.cy, FRootItemSize.cy);
end;

function TdxBreadcrumbEditDropDownInnerListBox.ItemAtPos(const APoint: TPoint; AExistOnly: Boolean = False): Integer;
var
  AItemRect, ASeparatorRect: TRect;
begin
  Result := inherited ItemAtPos(APoint, AExistOnly);
  if IsRootAlwaysVisible and (Result < 0) and (HotIndex = ItemIndex) then
  begin
    GetItemPartsRects(RootItem, RootItemRect, AItemRect, ASeparatorRect);
    if PtInRect(AItemRect, APoint) then
      RootItemState := cxbsPressed
    else
      RootItemState := cxbsNormal;
  end;
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.UpdateRoot;
begin
  if IsRootAlwaysVisible then
  begin
    FRootItemSize := Size(FRootItemSize.cx, ItemsHeight[0]);
    RootItem.Assign(Items[0]);
    Items.FreeAndDelete(0);
  end
  else
    FRootItemSize := cxNullSize;
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.AdjustItemFont(AFont: TFont; AItem: TdxCustomListBoxItem;
  AState: TcxButtonState);
begin
  if AItem <> RootItem then
    inherited AdjustItemFont(AFont, AItem, AState)
  else
    if DefaultItemIndex = -1 then
      AFont.Style := AFont.Style + [fsBold];
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.DoSelectItem(ASelectedViaKeyboard: Boolean);
begin
  if Items.IsValidIndex(ItemIndex) then
    inherited DoSelectItem(ASelectedViaKeyboard)
  else
    if IsRootAlwaysVisible and (RootItemState = cxbsPressed) then
    begin
      if Assigned(OnSelectItem) then
        OnSelectItem(Self, RootItem, ASelectedViaKeyboard);
    end;
end;

function TdxBreadcrumbEditDropDownInnerListBox.GetHasSeparator(AItem: TdxCustomListBoxItem): Boolean;
begin
  if AItem <> RootItem then
    Result := inherited GetHasSeparator(AItem)
  else
    Result := RootItem.HasSeparator;
end;

function TdxBreadcrumbEditDropDownInnerListBox.GetItemsAreaRect: TRect;
begin
  Result := cxRectInflate(ClientBounds, 0, -FRootItemSize.cy, 0, 0);
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.Paint;
begin
  inherited Paint;
  if IsRootAlwaysVisible then
    DrawItem(RootItemRect, RootItem, RootItemState);
end;

function TdxBreadcrumbEditDropDownInnerListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;
var
  AIsMoveToRoot, AIsMoveFromRoot: Boolean;
begin
  AIsMoveToRoot := ((ItemIndex = 0) and (Key = VK_UP)) or ((ItemIndex = (Items.Count - 1)) and (Key = VK_DOWN));
  AIsMoveFromRoot := (ItemIndex = -1) and (RootItemState = cxbsPressed) and
    (Key in [VK_UP, VK_DOWN, VK_END, VK_HOME, VK_NEXT, VK_PRIOR]);

  Result := True;
  if not (IsRootAlwaysVisible and (AIsMoveToRoot or AIsMoveFromRoot)) then
    Result := inherited ProcessNavigationKey(Key, Shift)
  else
    if AIsMoveToRoot then
    begin
      if LoopedNavigation or (Key = VK_UP) then
        RootItemState := cxbsPressed
      else
        Result := False;
    end
    else
      if Key = VK_UP then
      begin
        if LoopedNavigation then
          ItemIndex := Items.Count - 1
        else
          Result := False;
      end
      else
        if Key = VK_DOWN then
          ItemIndex := 0
        else
          Result := False;
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.SetItemIndex(AIndex: Integer);
begin
  inherited SetItemIndex(AIndex);
  if ItemIndex > -1 then
    RootItemState := cxbsNormal;
end;

function TdxBreadcrumbEditDropDownInnerListBox.GetIsRootAlwaysVisible: Boolean;
begin
  Result := TdxBreadcrumbEditNodeDropDownMenu(Parent).RootAlwaysVisible;
end;

function TdxBreadcrumbEditDropDownInnerListBox.GetRootItemRect: TRect;
begin
  Result := cxRectInflate(ClientRect, -BorderSize, -BorderSize);
  Result := cxRectSetHeight(Result, FRootItemSize.cy);
end;

procedure TdxBreadcrumbEditDropDownInnerListBox.SetRootItemState(AValue: TcxButtonState);
begin
  if FRootItemState <> AValue then
  begin
    FRootItemState := AValue;
    if RootItemState <> cxbsNormal then
      ItemIndex := -1;
    Invalidate;
  end;
end;

{ TdxBreadcrumbEditPathEditor }

constructor TdxBreadcrumbEditPathEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoCompleteSuggestions := TStringList.Create;
  FAutoCompleteSuggestions.OnChange := DoAutoCompleteSuggestionsChanged;
end;

destructor TdxBreadcrumbEditPathEditor.Destroy;
begin
  HideAutoCompleteWindow;
  FreeAndNil(FAutoCompleteSuggestions);
  inherited Destroy;
end;

function TdxBreadcrumbEditPathEditor.CanDropDown: Boolean;
begin
  Result := Properties.CanShowDropDown;
end;

function TdxBreadcrumbEditPathEditor.CanShowValidationErrorOnPostEditValue: Boolean;
begin
  Result := True;
end;

function TdxBreadcrumbEditPathEditor.CreateAutoCompleteWindow: TdxBreadcrumbEditAutoCompleteWindow;
begin
  Result := TdxBreadcrumbEditAutoCompleteWindow.Create(Self);
end;

function TdxBreadcrumbEditPathEditor.CreateViewData: TcxCustomEditViewData;
begin
  Result := inherited CreateViewData;
  Result.OnGetDefaultButtonWidth := DoGetDefaultButtonWidthEvent;
end;

procedure TdxBreadcrumbEditPathEditor.DoAutoCompleteSuggestionsChanged(Sender: TObject);
begin
  RefreshAutoCompleteWindow;
end;

procedure TdxBreadcrumbEditPathEditor.DoChange;
begin
  inherited DoChange;
  if FAutoCompleteSuggestionsUpdateLockCount = 0 then
    RefreshAutoCompleteSuggestions;
end;

procedure TdxBreadcrumbEditPathEditor.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      if AutoCompleteWindow <> nil then
      begin
        AutoCompleteWindow.ProcessNavigationKey(Key, Shift);
        Key := 0;
      end;

    VK_ESCAPE:
      if AutoCompleteWindow <> nil then
        HideAutoCompleteWindow
      else
        if DroppedDown then
          DroppedDown := False
        else
        begin
          Key := 0;
          SetFocusToParent;
        end;

    VK_UP, VK_DOWN:
      begin
        if AutoCompleteWindow <> nil then
        begin
          AutoCompleteWindow.ProcessNavigationKey(Key, Shift);
          PreviewEditValue(AutoCompleteWindow.SelectedText, False);
        end
        else
          if DroppedDown then
          begin
            inherited DoEditKeyDown(Key, Shift);
            PreviewEditValue(LookupData.CurrentValue, True);
          end
          else
            RefreshAutoCompleteSuggestions;

        Key := 0;
      end;
  end;

  inherited DoEditKeyDown(Key, Shift);
end;

procedure TdxBreadcrumbEditPathEditor.DoGetDefaultButtonWidthEvent(
  Sender: TcxCustomEditViewData; AIndex: Integer; var ADefaultWidth: Integer);
begin
  ADefaultWidth := Style.LookAndFeel.Painter.BreadcrumbEditScaledDropDownButtonWidth(ScaleFactor);
  dxAdjustToTouchableSize(ADefaultWidth, ScaleFactor);
end;

procedure TdxBreadcrumbEditPathEditor.DoPopulateAutoCompleteSuggestions;
begin
  if Assigned(OnPopulateAutoCompleteSuggestions) then
    OnPopulateAutoCompleteSuggestions(Self, EditingValue, AutoCompleteSuggestions);
end;

procedure TdxBreadcrumbEditPathEditor.DropDown;
begin
  HideAutoCompleteWindow;
  inherited DropDown;
end;

procedure TdxBreadcrumbEditPathEditor.HandleSelectItem(Sender: TObject);
begin
  Inc(FAutoCompleteSuggestionsUpdateLockCount);
  try
    inherited HandleSelectItem(Self);
  finally
    Dec(FAutoCompleteSuggestionsUpdateLockCount);
  end;
end;

procedure TdxBreadcrumbEditPathEditor.HideAutoCompleteWindow;
begin
  FreeAndNil(FAutoCompleteWindow);
end;

procedure TdxBreadcrumbEditPathEditor.PreviewEditValue(
  const AEditValue: string; ASelectAll: Boolean);
begin
  EditValue := AEditValue;
  if ASelectAll then
  begin
    SelStart := 0;
    SelLength := Length(AEditValue);
  end
  else
    SelStart := Length(AEditValue);
end;

procedure TdxBreadcrumbEditPathEditor.SetFocusToParent;
begin
  if Parent.CanFocus then
    Parent.SetFocus;
end;

procedure TdxBreadcrumbEditPathEditor.StoreAutoCompleteWindowCustomizedSize;
begin
  if AutoCompleteWindow <> nil then
    FAutoCompleteWindowCustomizedSize := cxSize(AutoCompleteWindow.BoundsRect);
end;

procedure TdxBreadcrumbEditPathEditor.RefreshAutoCompleteSuggestions;
begin
  AutoCompleteSuggestions.BeginUpdate;
  try
    AutoCompleteSuggestions.Clear;
    DoPopulateAutoCompleteSuggestions;
  finally
    AutoCompleteSuggestions.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditPathEditor.RefreshAutoCompleteWindow;
begin
  if Properties.AutoComplete and (AutoCompleteSuggestions.Count > 0) then
  begin
    DroppedDown := False;
    if AutoCompleteWindow = nil then
    begin
      FAutoCompleteWindow := CreateAutoCompleteWindow;
      FAutoCompleteWindow.ItemsFont := Style.Font;
      FAutoCompleteWindow.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
      FAutoCompleteWindow.OnSelectItem := SelectAutoCompleteWindowItemHandler;
      FAutoCompleteWindow.OnStoreSize := StoreAutoCompleteWindowSize;
      FAutoCompleteWindow.DisplayRowsCount := Properties.AutoCompleteDropDownRows;
      FAutoCompleteWindow.BiDiMode := BiDiMode;
    end;
    AutoCompleteWindow.Adjustable := not AutoCompleteWindowCustomizedSizeAssigned;
    if not AutoCompleteWindow.Adjustable then
      AutoCompleteWindow.SetSize(AutoCompleteWindowCustomizedSize);
    AutoCompleteWindow.SearchText := EditingValue;
    AutoCompleteWindow.Populate(AutoCompleteSuggestions);
    AutoCompleteWindow.OwnerBounds := InnerControl.BoundsRect;
    AutoCompleteWindow.Popup(InnerControl);
  end
  else
    HideAutoCompleteWindow;
end;

procedure TdxBreadcrumbEditPathEditor.WndProc(var Message: TMessage);
begin
  if Message.Msg = DXM_BREADCRUMBEDIT_HIDESUGGESTIONS then
    HideAutoCompleteWindow;
  inherited WndProc(Message);
end;

function TdxBreadcrumbEditPathEditor.GetActiveProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
begin
  Result := TdxBreadcrumbEditPathInplaceEditorProperties(inherited ActiveProperties);
end;

class function TdxBreadcrumbEditPathEditor.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TdxBreadcrumbEditPathEditorDataBinding;
end;

function TdxBreadcrumbEditPathEditor.GetIsAutoCompleteWindowCustomizedSizeAssigned: Boolean;
begin
  Result := (AutoCompleteWindowCustomizedSize.cx > 0) and (AutoCompleteWindowCustomizedSize.cy > 0);
end;

function TdxBreadcrumbEditPathEditor.GetLookupData: TdxBreadcrumbEditPathEditorLookupData;
begin
  Result := TdxBreadcrumbEditPathEditorLookupData(inherited LookupData);
end;

function TdxBreadcrumbEditPathEditor.GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
begin
  Result := TdxBreadcrumbEditPathInplaceEditorProperties(inherited Properties);
end;

class function TdxBreadcrumbEditPathEditor.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxBreadcrumbEditPathInplaceEditorProperties;
end;

function TdxBreadcrumbEditPathEditor.GetScrollLookupDataList(AScrollCause: TcxEditScrollCause): Boolean;
begin
  Result := AScrollCause = escKeyboard;
end;

procedure TdxBreadcrumbEditPathEditor.SetEditValue(const AValue: TcxEditValue);
begin
  Inc(FAutoCompleteSuggestionsUpdateLockCount);
  try
    inherited SetEditValue(AValue);
  finally
    Dec(FAutoCompleteSuggestionsUpdateLockCount);
  end;
end;

procedure TdxBreadcrumbEditPathEditor.SelectAutoCompleteWindowItemHandler(Sender: TObject);
begin
  EditValue := FAutoCompleteWindow.SelectedText;
  PostEditValue;
end;

procedure TdxBreadcrumbEditPathEditor.SetAutoCompleteSuggestions(AValue: TStringList);
begin
  FAutoCompleteSuggestions.Assign(AValue);
end;

procedure TdxBreadcrumbEditPathEditor.StoreAutoCompleteWindowSize(Sender: TObject);
begin
  StoreAutoCompleteWindowCustomizedSize;
end;

{ TdxBreadcrumbEditPathEditorDataBinding }

function TdxBreadcrumbEditPathEditorDataBinding.CanPostEditorValue: Boolean;
begin
  Result := True;
end;

{ TdxBreadcrumbEditPathEditorViewInfo }

procedure TdxBreadcrumbEditPathEditorViewInfo.DrawEditButton(ACanvas: TcxCanvas; AButtonVisibleIndex: Integer);
const
  StatesMap: array[TcxEditButtonState] of TdxBreadcrumbEditButtonState =
    (dxbcbsDisabled, dxbcbsNormal, dxbcbsPressed, dxbcbsHot);
var
  AButtonInfo: TcxEditButtonViewInfo;
begin
  AButtonInfo := ButtonsInfo[AButtonVisibleIndex];
  Painter.DrawBreadcrumbEditScaledDropDownButton(ACanvas,
    AButtonInfo.Bounds, StatesMap[AButtonInfo.Data.State], True, ScaleFactor);
  Painter.DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas,
    AButtonInfo.Bounds, StatesMap[AButtonInfo.Data.State], True, ScaleFactor);
end;

procedure TdxBreadcrumbEditPathEditorViewInfo.GetColorSettingsByPainter(out ABackgroundColor, ATextColor: TColor);
begin
  TdxBreadcrumbEditPathEditor(Edit).GetColorSettingsByPainter(ABackgroundColor, ATextColor);
end;

procedure TdxBreadcrumbEditPathEditorViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  inherited InternalPaint(ACanvas);
  if Images <> nil then
    cxDrawImage(ACanvas.Handle, ImageRect, ImageRect, nil, Images, ImageIndex, idmNormal);
end;

procedure TdxBreadcrumbEditPathEditorViewInfo.Offset(DX: Integer; DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(ImageRect, DX, DY);
end;

{ TdxBreadcrumbEditPathEditorViewData }

procedure TdxBreadcrumbEditPathEditorViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AImageAreaSize: Integer;
  AViewInfo: TdxBreadcrumbEditPathEditorViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, ViewInfo, AIsMouseEvent);
  AViewInfo := TdxBreadcrumbEditPathEditorViewInfo(ViewInfo);
  AViewInfo.ImageIndex := Properties.ImageIndex;
  AViewInfo.Images := Properties.Images;
  if AViewInfo.Images <> nil then
  begin
    AImageAreaSize := dxGetImageSize(AViewInfo.Images, ScaleFactor).cx + 2 * dxBreadcrumbEditImageBorder;
    AViewInfo.ImageRect := cxRectSetWidth(AViewInfo.ClientRect, AImageAreaSize);
    AViewInfo.ImageRect := cxRectCenter(AViewInfo.ImageRect, dxGetImageSize(AViewInfo.Images, ScaleFactor));
    if UseRightToLeftAlignment then
    begin
      AViewInfo.ImageRect := TdxRightToLeftLayoutConverter.ConvertRect(AViewInfo.ImageRect, AViewInfo.ClientRect);
      Dec(AViewInfo.ClientRect.Right, AImageAreaSize);
      Dec(AViewInfo.TextRect.Right, AImageAreaSize);
    end
    else
    begin
      Inc(AViewInfo.ClientRect.Left, AImageAreaSize);
      Inc(AViewInfo.TextRect.Left, AImageAreaSize);
    end;
  end;
end;

procedure TdxBreadcrumbEditPathEditorViewData.CalculateButtonNativeInfo(AButtonViewInfo: TcxEditButtonViewInfo);
begin
  inherited CalculateButtonNativeInfo(AButtonViewInfo);
  AButtonViewInfo.Data.BackgroundPartiallyTransparent := True;
end;

function TdxBreadcrumbEditPathEditorViewData.InternalGetEditConstantPartSize(
  ACanvas: TcxCanvas; AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  AImageSize: TSize;
  APathEditorViewInfo: TdxBreadcrumbEditPathEditorViewInfo;
begin
  Result := inherited InternalGetEditConstantPartSize(
    ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);

  APathEditorViewInfo := TdxBreadcrumbEditPathEditorViewInfo(AViewInfo);
  if Assigned(APathEditorViewInfo.Images) then
  begin
    AImageSize := dxGetImageSize(APathEditorViewInfo.Images, ScaleFactor);
    MinContentSize.cy := Max(MinContentSize.cy, AImageSize.cy);
    Inc(Result.cx, AImageSize.cx + ScaleFactor.Apply(5));
  end
end;

function TdxBreadcrumbEditPathEditorViewData.GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
begin
  Result := TdxBreadcrumbEditPathInplaceEditorProperties(inherited Properties);
end;

{ TdxBreadcrumbEditPathEditorListBox }

procedure TdxBreadcrumbEditPathEditorListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AImageRect: TRect;
  AText: string;
begin
  if not DoDrawItem(Index, Rect, State) then
  begin
    Canvas.FillRect(Rect);
    if (Index >= 0) and (Index < Items.Count) then
    begin
      AImageRect := GetImageRect(Rect);
      if Images <> nil then
        cxDrawImage(Canvas.Handle, AImageRect, AImageRect, nil, Images, ImageIndex[Index], idmNormal);
      if UseRightToLeftAlignment then
        Rect.Right := AImageRect.Left
      else
        Rect.Left := AImageRect.Right;

      AText := GetItem(Index);
      Rect := cxRectInflate(Rect, -cxTextOffset);
      DrawText(Canvas.Handle, PChar(AText), Length(AText), Rect,
        DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
    end;
  end;
end;

function TdxBreadcrumbEditPathEditorListBox.GetImageIndex(AItemIndex: Integer): TcxImageIndex;
begin
  Result := TcxImageIndex(Edit.ActiveProperties.Items.Objects[AItemIndex]);
end;

function TdxBreadcrumbEditPathEditorListBox.GetImageRect(const R: TRect): TRect;
var
  AImageSize: TSize;
begin
  if Images <> nil then
  begin
    AImageSize := dxGetImageSize(Images, ScaleFactor);
    Result := cxRectSetWidth(R, 2 * dxBreadcrumbEditImageBorder + AImageSize.cx);
    Result := cxRectCenter(Result, AImageSize);
    if UseRightToLeftAlignment then
      Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, R);
  end
  else
    Result := cxNullRect;
end;

function TdxBreadcrumbEditPathEditorListBox.GetItemHeight(AIndex: Integer = -1): Integer;
begin
  Result := inherited GetItemHeight(AIndex);
  if Images <> nil then
    Result := Max(Result, 2 * dxBreadcrumbEditImageBorder + dxGetImageSize(Images, ScaleFactor).cy);
end;

function TdxBreadcrumbEditPathEditorListBox.GetImages: TCustomImageList;
begin
  Result := Properties.Images;
end;

function TdxBreadcrumbEditPathEditorListBox.GetProperties: TdxBreadcrumbEditPathInplaceEditorProperties;
begin
  Result := TdxBreadcrumbEditPathEditor(Edit).ActiveProperties;
end;

{ TdxBreadcrumbEditPathEditorLookupData }

function TdxBreadcrumbEditPathEditorLookupData.GetCurrentValue: string;
begin
  if ItemIndex >= 0 then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

function TdxBreadcrumbEditPathEditorLookupData.GetListBoxClass: TcxCustomEditListBoxClass;
begin
  Result := TdxBreadcrumbEditPathEditorListBox;
end;

{ TdxBreadcrumbEditAutoCompleteWindow }

procedure TdxBreadcrumbEditAutoCompleteWindow.DoClosed;
begin
  inherited DoClosed;
  PostMessage(OwnerControl.Handle, DXM_BREADCRUMBEDIT_HIDESUGGESTIONS, 0, 0);
end;

{ TdxBreadcrumbEditPathInplaceEditorProperties }

constructor TdxBreadcrumbEditPathInplaceEditorProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoCompleteDropDownRows := dxBreadcrumbEditDefaultAutoCompleteDropDownRows;
  FCanShowDropDown := True;
  FAutoComplete := True;
  FImageIndex := -1;
end;

function TdxBreadcrumbEditPathInplaceEditorProperties.CanValidate: Boolean;
begin
  Result := False;
end;

class function TdxBreadcrumbEditPathInplaceEditorProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TdxBreadcrumbEditPathEditorLookupData;
end;

class function TdxBreadcrumbEditPathInplaceEditorProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxBreadcrumbEditPathEditorViewData;
end;

class function TdxBreadcrumbEditPathInplaceEditorProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxBreadcrumbEditPathEditorViewInfo;
end;

procedure TdxBreadcrumbEditPathInplaceEditorProperties.SetAutoComplete(AValue: Boolean);
begin
  if AValue <> FAutoComplete then
  begin
    FAutoComplete := AValue;
    Changed;
  end;
end;

procedure TdxBreadcrumbEditPathInplaceEditorProperties.SetAutoCompleteDropDownRows(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if AValue <> FAutoCompleteDropDownRows then
  begin
    FAutoCompleteDropDownRows := AValue;
    Changed;
  end;
end;

procedure TdxBreadcrumbEditPathInplaceEditorProperties.SetCanShowDropDown(AValue: Boolean);
begin
  FCanShowDropDown := AValue;
  Buttons.Items[0].Visible := CanShowDropDown;
end;

procedure TdxBreadcrumbEditPathInplaceEditorProperties.SetImageIndex(AValue: TcxImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

{ TdxBreadcrumbEditPathEditingController }

constructor TdxBreadcrumbEditPathEditingController.Create(AController: TdxBreadcrumbEditController);
begin
  inherited Create;
  FController := AController;
end;

destructor TdxBreadcrumbEditPathEditingController.Destroy;
begin
  ClosePathEditor;
  inherited Destroy;
end;

procedure TdxBreadcrumbEditPathEditingController.AddToRecentPaths(ANode: TdxBreadcrumbEditNode);
var
  AIndex: Integer;
begin
  AIndex := RecentPaths.IndexOfPath(ANode.Path);
  if AIndex >= 0 then
    RecentPaths[AIndex].Index := 0
  else
  begin
    RecentPaths.Insert(0, ANode.Path, ANode.ImageIndex);
    CheckRecentPathsCount;
  end;
end;

function TdxBreadcrumbEditPathEditingController.CanAddToRecentPaths(ANode: TdxBreadcrumbEditNode): Boolean;
begin
  Result := Properties.RecentPathsEnabled and Properties.RecentPathsAutoPopulate;
end;

procedure TdxBreadcrumbEditPathEditingController.CheckRecentPathsCount;
var
  AMaxCount: Integer;
begin
  AMaxCount := Properties.RecentPathsMaxCount;
  if AMaxCount > 0 then
  begin
    RecentPaths.BeginUpdate;
    try
      while RecentPaths.Count > AMaxCount do
        RecentPaths.Delete(RecentPaths.Count - 1);
    finally
      RecentPaths.EndUpdate;
    end;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.ClosePathEditor;
begin
  if PathEditor <> nil then
  begin
    FreeAndNil(FPathEditor);
    Controller.UpdateState;
  end;
end;

function TdxBreadcrumbEditPathEditingController.DoPathEntered(var ANewPath: string): Boolean;
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  Result := False;
  if Supports(Controller.Control, IdxBreadcrumbEditEvents, AEvents) then
  try
    AEvents.PathEntered(ANewPath, Result);
  finally
    AEvents := nil;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.DropDown;
begin
  OpenPathEditor;
  if PathEditor <> nil then
    PathEditor.DropDown;
end;

procedure TdxBreadcrumbEditPathEditingController.OpenPathEditor;
begin
  if Properties.Enabled then
  begin
    if FPathEditor = nil then
    begin
      FPathEditor := TdxBreadcrumbEditPathEditor.Create(nil, True);
      SetupPathEditorStyles;
    {$IFNDEF VCLGLASSPAINT}
      PathEditor.OnGlass := Control.GetOnGlass;
    {$ENDIF}
      PathEditor.Parent := Control.GetContainer;
      PathEditor.OnPopulateAutoCompleteSuggestions := PathEditorPopulateAutoCompleteSuggestions;
      PathEditor.OnPostEditValue := PathEditorPostEditValueHandler;
      PathEditor.OnEnter := PathEditorEnterHandler;
      PathEditor.OnExit := PathEditorExitHandler;
      PopulateRecentPaths(PathEditor.Properties.Items);
      SetupPathEditorProperties;
    end;
    if Selected <> nil then
    begin
      PathEditor.EditValue := Selected.Path;
      PathEditor.Properties.ImageIndex := Selected.ImageIndex;
    end;
    UpdatePathEditorBounds;
    PathEditor.SelectAll;
    PathEditor.SetFocus;
    Controller.UpdateState;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.PathEditorEnterHandler(Sender: TObject);
begin
  Controller.UpdateState;
end;

procedure TdxBreadcrumbEditPathEditingController.PathEditorExitHandler(Sender: TObject);
begin
  ReleasePathEditor;
end;

procedure TdxBreadcrumbEditPathEditingController.PathEditorPopulateAutoCompleteSuggestions(
  Sender: TObject; const APath: string; ASuggestions: TStringList);
begin
  PopulateSuggestions(APath, ASuggestions);
end;

procedure TdxBreadcrumbEditPathEditingController.PathEditorPostEditValueHandler(Sender: TObject);
var
  ANewPath: string;
begin
  if not VarIsNull(PathEditor.EditValue) then
  begin
    ANewPath := PathEditor.EditValue;
    if not DoPathEntered(ANewPath) and (ANewPath <> '') then
    begin
      if Controller.SelectPath(ANewPath) then
      begin
        if CanAddToRecentPaths(Selected) then
          AddToRecentPaths(Selected);
      end;
    end;
    PathEditor.SetFocusToParent;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.AddSuggestion(
  const ACurrentPath, ASuggestedPath: string; ASuggestions: TStringList);
begin
  if IsSubPath(ACurrentPath, ASuggestedPath) then
  begin
    if ASuggestions.IndexOf(ASuggestedPath) < 0 then
      ASuggestions.Add(ASuggestedPath);
  end;
end;

function TdxBreadcrumbEditPathEditingController.IsSubPath(const ABasePath, APathForCheck: string): Boolean;
begin
  Result := (Length(APathForCheck) > Length(ABasePath)) and SameText(ABasePath, Copy(APathForCheck, 1, Length(ABasePath)));
end;

function TdxBreadcrumbEditPathEditingController.CanAddNodeToSuggestions(
  const ACurrentPath: string; ANode: TdxBreadcrumbEditNode): Boolean;
begin
  Result := not ANode.IsHidden;
end;

procedure TdxBreadcrumbEditPathEditingController.PopulateSuggestions(const APath: string; ASuggestions: TStringList);

  function GetCurrentNode(const APath: string): TdxBreadcrumbEditNode;
  begin
    if APath = '' then
      Result := Controller.Root
    else
      Result := Controller.FindNodeByPath(APath);
  end;

  procedure PopulateSuggestionsFromRecentlyUsed;
  var
    I: Integer;
  begin
    for I := 0 to PathEditor.Properties.Items.Count - 1 do
      AddSuggestion(APath, PathEditor.Properties.Items[I], ASuggestions);
  end;

  procedure PopulateSuggestionsFromNodeChildren(const ACompletePath: string; ANode: TdxBreadcrumbEditNode);
  var
    AChildNode: TdxBreadcrumbEditNode;
    I: Integer;
  begin
    if ANode <> nil then
    begin
      ANode.LoadChildren;
      for I := 0 to ANode.Count - 1 do
      begin
        AChildNode := ANode[I];
        if CanAddNodeToSuggestions(APath, AChildNode) then
        begin
          if IsSubPath(APath, AChildNode.Path) then
            AddSuggestion(APath, AChildNode.Path, ASuggestions)
          else
          begin
            AddSuggestion(APath, ACompletePath + AChildNode.Name, ASuggestions);
            if AChildNode.DisplayName <> '' then
              AddSuggestion(APath, ACompletePath + AChildNode.DisplayName, ASuggestions);
          end;
        end;
      end;
    end;
  end;

var
  ACompletePath: string;
begin
  if APath <> '' then
  begin
    if Properties.RecentPathsEnabled then
      PopulateSuggestionsFromRecentlyUsed;
    ACompletePath := Copy(APath, 1, LastDelimiter(Properties.PathDelimiter, APath));
    PopulateSuggestionsFromNodeChildren(ACompletePath, GetCurrentNode(ACompletePath));
    ASuggestions.Sort;
  end;
  PopulateCustomSuggestions(APath, ASuggestions);
end;

procedure TdxBreadcrumbEditPathEditingController.PopulateCustomSuggestions(const APath: string; ASuggestions: TStringList);
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  if Supports(Control, IdxBreadcrumbEditEvents, AEvents) then
  try
    AEvents.PopulateAutoCompleteSuggestions(APath, ASuggestions);
  finally
    AEvents := nil;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.PopulateRecentPaths(AItems: TStrings);
var
  ARecentPath: TdxBreadcrumbEditRecentPath;
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to Properties.RecentPaths.Count - 1 do
    begin
      ARecentPath := Properties.RecentPaths[I];
      if Trim(ARecentPath.Path) <> '' then
        AItems.AddObject(ARecentPath.Path, TObject(ARecentPath.ImageIndex));
    end;
  finally
    AItems.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditPathEditingController.ReleasePathEditor;
begin
  PostMessage(Controller.Handle, DXM_BREADCRUMBEDIT_RELEASEPATHEDITOR, 0, 0);
end;

procedure TdxBreadcrumbEditPathEditingController.SetupPathEditorProperties;
begin
  PathEditor.Properties.Alignment.Vert := taVCenter;
  PathEditor.Properties.AutoComplete := Properties.AutoComplete;
  PathEditor.Properties.AutoCompleteDropDownRows := Properties.AutoCompleteDropDownRows;
  PathEditor.Properties.CanShowDropDown := Properties.RecentPathsEnabled;
  PathEditor.Properties.DropDownListStyle := lsEditList;
  PathEditor.Properties.DropDownRows := Properties.RecentPathsDropDownRows;
  PathEditor.Properties.Images := ViewInfo.Properties.Images;
  PathEditor.Properties.ImmediateDropDownWhenKeyPressed := False;
  PathEditor.Properties.ImmediatePost := True;
  PathEditor.Properties.IncrementalSearch := False;
  PathEditor.Properties.ReadOnly := Properties.ReadOnly;
end;

procedure TdxBreadcrumbEditPathEditingController.SetupPathEditorStyles;
begin
  PathEditor.Style.LookAndFeel.MasterLookAndFeel := Control.GetLookAndFeel;
  if Control.GetFont.Color <> clWindowText then
    PathEditor.Style.TextColor := Control.GetFont.Color;
end;

procedure TdxBreadcrumbEditPathEditingController.UpdatePathEditorBounds;
begin
  if PathEditor <> nil then
    PathEditor.BoundsRect := ViewInfo.NodesAreaViewInfo.PathEditorRect;
end;

function TdxBreadcrumbEditPathEditingController.GetControl: IdxBreadcrumbEdit;
begin
  Result := Controller.Control;
end;

function TdxBreadcrumbEditPathEditingController.GetIsFocused: Boolean;
begin
  Result := (PathEditor <> nil) and PathEditor.Focused;
end;

function TdxBreadcrumbEditPathEditingController.GetProperties: TdxBreadcrumbEditPathEditorProperties;
begin
  Result := ViewInfo.Properties.PathEditor;
end;

function TdxBreadcrumbEditPathEditingController.GetRecentPaths: TdxBreadcrumbEditRecentPaths;
begin
  Result := Properties.RecentPaths;
end;

function TdxBreadcrumbEditPathEditingController.GetSelected: TdxBreadcrumbEditNode;
begin
  Result := Controller.Selected;
end;

function TdxBreadcrumbEditPathEditingController.GetViewInfo: TdxBreadcrumbEditViewInfo;
begin
  Result := Controller.ViewInfo;
end;

{ TdxBreadcrumbEditController }

constructor TdxBreadcrumbEditController.Create(AViewInfo: TdxBreadcrumbEditViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FPathEditingController := CreatePathEditingController;
  FProgressBarController := CreateProgressBarController;
end;

destructor TdxBreadcrumbEditController.Destroy;
begin
  FreeAndNil(FPathEditingController);
  FreeAndNil(FProgressBarController);
  FreeAndNil(FDropDownMenuWindow);
  inherited Destroy;
end;

procedure TdxBreadcrumbEditController.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FDestroying := True;
end;

procedure TdxBreadcrumbEditController.BeginUpdate;
begin
  Inc(FUpdateLockCount);
end;

procedure TdxBreadcrumbEditController.EndUpdate;
begin
  Dec(FUpdateLockCount);
  if FUpdateLockCount = 0 then
    Changed([]);
end;

procedure TdxBreadcrumbEditController.BoundsChanged(const ARect: TRect);
begin
  ViewInfo.Calculate(ARect);
  PathEditingController.UpdatePathEditorBounds;
end;

function TdxBreadcrumbEditController.CanClickAt(AViewItem: TdxBreadcrumbEditViewItem): Boolean;
begin
  Result := (AViewItem <> nil) and AViewItem.GetIsEnabled;
end;

procedure TdxBreadcrumbEditController.Changed(const AChanges: TdxBreadcrumbEditChanges);
var
  AChangesForProcessing: TdxBreadcrumbEditChanges;
begin
  FChanges := FChanges + AChanges;
  if (FChanges <> []) and not (Destroying or IsUpdateLocked) then
  begin
    AChangesForProcessing := FChanges;
    FChanges := [];
    ProcessChanges(AChangesForProcessing);
    UpdateHitTest;
    ViewInfo.Invalidate;
  end;
end;

function TdxBreadcrumbEditController.CalculateState: TdxBreadcrumbEditState;
begin
  if not Control.GetIsEnabled then
    Result := dxbcsDisabled
  else
    if MouseInControl then
      Result := dxbcsHot
    else
      if IsFocused then
        Result := dxbcsFocused
      else
        Result := dxbcsNormal;
end;

function TdxBreadcrumbEditController.CanOpenPathEditorOnEnter: Boolean;
begin
  Result := (FocusedViewItem = nil) or (FocusedViewItem is TdxBreadcrumbEditNodeViewItem) and
    (TdxBreadcrumbEditNodeViewItem(FocusedViewItem).Node = Root);
end;

function TdxBreadcrumbEditController.CanSelectNode(ANode: TdxBreadcrumbEditNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.HasAsParent(Root) or (ANode = Root));
end;

function TdxBreadcrumbEditController.CreateDropDownMenuWindow: TdxBreadcrumbEditNodeDropDownMenu;
begin
  Result := TdxBreadcrumbEditNodeDropDownMenu.Create(Self);
end;

function TdxBreadcrumbEditController.CreatePathEditingController: TdxBreadcrumbEditPathEditingController;
begin
  Result := TdxBreadcrumbEditPathEditingController.Create(Self);
end;

function TdxBreadcrumbEditController.CreateProgressBarController: TdxBreadcrumbEditProgressBarController;
begin
  Result := TdxBreadcrumbEditProgressBarController.Create(Self);
end;

procedure TdxBreadcrumbEditController.DoAfterSelect;
begin
end;

procedure TdxBreadcrumbEditController.DoBeforeSelect;
begin
  PathEditingController.ReleasePathEditor;
end;

procedure TdxBreadcrumbEditController.DoDeleteNode(ANode: TdxBreadcrumbEditNode);

  function GetNextSelectableNode(ANode: TdxBreadcrumbEditNode): TdxBreadcrumbEditNode;
  begin
    Result := ANode;
    while (Result <> nil) and (nsDeleting in Result.State) do
      Result := Result.Parent;
    if Result = nil then
      Result := Root;
  end;

begin
  if ANode = Selected then
    Selected := GetNextSelectableNode(ANode.Parent);
  HideNodeDropDownMenu;
end;

procedure TdxBreadcrumbEditController.DoNodeDropDownPopup(ANode: TdxBreadcrumbEditNode);
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  if Supports(Control, IdxBreadcrumbEditEvents, AEvents) then
  try
    AEvents.NodeDropDownPopup(ANode);
  finally
    AEvents := nil;
  end;
end;

function TdxBreadcrumbEditController.DoSelectPath(const APath: string; ANode: TdxBreadcrumbEditNode): Boolean;
begin
  ValidatePath(APath, ANode);
  Selected := ANode;
  Result := Selected = ANode;
end;

procedure TdxBreadcrumbEditController.DoValidatePath(const APath: string;
  var ANode: TdxBreadcrumbEditNode; var AErrorText: string; var AError: Boolean);
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  if Supports(Control, IdxBreadcrumbEditEvents, AEvents) then
  try
    AEvents.PathValidate(APath, ANode, AErrorText, AError);
  finally
    AEvents := nil;
  end;
end;

function TdxBreadcrumbEditController.FindNextSelectableViewItem(
  AItem: TdxBreadcrumbEditViewItem): TdxBreadcrumbEditViewItem;
begin
  Result := FindSelectableViewItem(AItem, True);
end;

function TdxBreadcrumbEditController.FindPrevSelectableViewItem(
  AItem: TdxBreadcrumbEditViewItem): TdxBreadcrumbEditViewItem;
begin
  Result := FindSelectableViewItem(AItem, False);
end;

function TdxBreadcrumbEditController.FindSelectableViewItem(
  AItem: TdxBreadcrumbEditViewItem; AGoForward: Boolean): TdxBreadcrumbEditViewItem;
var
  AIndex: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    Result := nil;
    ViewInfo.GetNavigationOrder(AList);
    if AList.Count > 0 then
    begin
      AIndex := AList.IndexOf(AItem) + IfThen(AGoForward, 1, -1);
      if AIndex < 0 then
        AIndex := AList.Count - 1;
      if AIndex >= AList.Count then
        AIndex := 0;
      Result := TdxBreadcrumbEditViewItem(AList[AIndex]);
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxBreadcrumbEditController.FocusChanged;
begin
  if IsFocused then
  begin
    if (FocusedViewItem = nil) and not MouseInControl then
      FocusedViewItem := ViewInfo.NodesAreaViewInfo.FirstViewItem;
  end
  else
    if not IsNodeDropDownMenuWindowActive then
      FocusedViewItem := nil;

  UpdateState;
end;

function TdxBreadcrumbEditController.FindNodeByPath(APath: string): TdxBreadcrumbEditNode;
var
  ANodeName: string;
begin
  Result := nil;
  if FindRootNodeForPath(APath, Result) then
  begin
    while ParsePath(ANodeName, APath) do
      if not Result.FindNode(ANodeName, Result) then
      begin
        Result := nil;
        Break;
      end;
  end;
end;

function TdxBreadcrumbEditController.FindRootNodeForPath(var APath: string; out ANode: TdxBreadcrumbEditNode): Boolean;
var
  ANodeName: string;
begin
  Result := ParsePath(ANodeName, APath);
  if Result then
  begin
    if Root.Compare(ANodeName) then
      ANode := Root
    else
      if (Selected = nil) or not Selected.FindNode(ANodeName, ANode) then
        Result := Root.FindNode(ANodeName, ANode);
  end;
end;

function TdxBreadcrumbEditController.IsNodeDropDownMenuWindowActive: Boolean;
begin
  Result := DropDownMenuWindow <> nil;
end;

procedure TdxBreadcrumbEditController.HideNodeDropDownMenu;
begin
  if not FLockDropDownMenuWindowDestroying then
    FreeAndNil(FDropDownMenuWindow);
  DropDownMenuOwner := nil;
end;

procedure TdxBreadcrumbEditController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F4:
      PathEditingController.DropDown;
    VK_LEFT, VK_RIGHT:
      if (Key = VK_LEFT) <> ControlContainer.UseRightToLeftAlignment then
        FocusedViewItem := FindPrevSelectableViewItem(FocusedViewItem)
      else
        FocusedViewItem := FindNextSelectableViewItem(FocusedViewItem);

    VK_UP, VK_DOWN:
      if FocusedViewItem is TdxBreadcrumbEditNodeViewItem then
        ShowNodeDropDownMenu(TdxBreadcrumbEditNodeViewItem(FocusedViewItem));
    VK_RETURN:
      if CanOpenPathEditorOnEnter then
        PathEditingController.OpenPathEditor
      else
        if CanClickAt(FocusedViewItem) then
          FocusedViewItem.Click;
  end;
end;

procedure TdxBreadcrumbEditController.MouseDown(
  AButton: TMouseButton; const P: TPoint; AShift: TShiftState);
begin
  if AButton = mbLeft then
  begin
    ViewInfo.CalculateHitTest(P);
    if HitTestInfo.ViewItem = nil then
      PathEditingController.OpenPathEditor;
    if (HitTestInfo.ViewItemPart = nvipDelimiter) and not IsNodeDropDownMenuWindowActive and
       (HitTestInfo.ViewItem is TdxBreadcrumbEditNodeViewItem)
    then
      ShowNodeDropDownMenu(TdxBreadcrumbEditNodeViewItem(HitTestInfo.ViewItem));
    PressedViewItem := HitTestInfo.ViewItem;
  end;
end;

procedure TdxBreadcrumbEditController.MouseMove(const P: TPoint; AShift: TShiftState);
begin
  if not cxPointIsEqual(FPrevMousePosition, P) then
  begin
    FPrevMousePosition := P;
    ViewInfo.CalculateHitTest(P);
    FocusedViewItem := HitTestInfo.ViewItem;
    if IsNodeDropDownMenuWindowActive and (HitTestInfo.ViewItem is TdxBreadcrumbEditNodeViewItem) then
    begin
      if DropDownMenuOwner <> HitTestInfo.ViewItem then
        ShowNodeDropDownMenu(TdxBreadcrumbEditNodeViewItem(HitTestInfo.ViewItem));
    end;
    MouseInControl := True;
  end;
end;

procedure TdxBreadcrumbEditController.MouseLeave;
begin
  FPrevMousePosition := cxInvalidPoint;
  UpdateHitTest;
end;

procedure TdxBreadcrumbEditController.MouseUp(
  AButton: TMouseButton; const P: TPoint; AShift: TShiftState);
begin
  if PressedViewItem <> nil then
  try
    ViewInfo.CalculateHitTest(P);
    if PressedViewItem = HitTestInfo.ViewItem then
    begin
      if CanClickAt(PressedViewItem) then
        PressedViewItem.Click(HitTestInfo.ViewItemPart);
    end;
  finally
    PressedViewItem := nil;
  end;
end;

function TdxBreadcrumbEditController.ParsePath(var ANodeName, ASubPath: string): Boolean;
var
  APathDelimPosition: Integer;
begin
  Result := ASubPath <> '';
  APathDelimPosition := Pos(PathEditingController.Properties.PathDelimiter, ASubPath);
  if APathDelimPosition = 0 then
    APathDelimPosition := Length(ASubPath) + 1;
  ANodeName := Copy(ASubPath, 1, APathDelimPosition - 1);
  ASubPath := Copy(ASubPath, APathDelimPosition + 1, MaxInt);
end;

procedure TdxBreadcrumbEditController.ProcessButtonClick(AButton: TdxBreadcrumbEditButton);
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  if AButton.ActionLink <> nil then
    AButton.ActionLink.Execute(ControlContainer)
  else
    if Supports(Control, IdxBreadcrumbEditEvents, AEvents) then
      AEvents.ProcessButtonClick(AButton);

  UpdateHitTest;
end;

procedure TdxBreadcrumbEditController.ProcessChanges(const AChanges: TdxBreadcrumbEditChanges);
begin
  if bcecPathEditor in AChanges then
  begin
    PathEditingController.ClosePathEditor;
    PathEditingController.CheckRecentPathsCount;
  end;
  if bcecButtons in AChanges then
    ViewInfo.ButtonsViewInfo.RecreateViewItems;
  if bcecNodes in AChanges then
    ViewInfo.NodesAreaViewInfo.RecreateViewItems;
  if [bcecButtons, bcecNodes] * AChanges <> [] then
    ValidateItems;
  if bcecLayout in AChanges then
    BoundsChanged(ViewInfo.Bounds);
  if bcecProgress in AChanges then
    ProgressBarController.UpdateProgress;
  if bcecAnimation in AChanges then
    ProgressBarController.UpdateOverlayAnimationState;
  if bcecLayout in AChanges then
    Control.AdjustAutoSize;
  if bcecSelection in AChanges then
    SelectionChanged;
end;

procedure TdxBreadcrumbEditController.SelectionChanged;
var
  AEvents: IdxBreadcrumbEditEvents;
begin
  if Supports(Control, IdxBreadcrumbEditEvents, AEvents) then
  try
    AEvents.SelectionChanged;
  finally
    AEvents := nil;
  end;
end;

function TdxBreadcrumbEditController.SelectPath(ANode: TdxBreadcrumbEditNode): Boolean;
begin
  Result := (ANode <> nil) and DoSelectPath(ANode.Path, ANode);
end;

function TdxBreadcrumbEditController.SelectPath(const APath: string): Boolean;
begin
  Result := DoSelectPath(APath, FindNodeByPath(APath));
end;

procedure TdxBreadcrumbEditController.SelectPathViaDropDownMenu(AItem: TdxBreadcrumbEditNode);
begin
  FLockDropDownMenuWindowDestroying := True;
  try
    SelectPath(AItem);
    if DropDownMenuWindow <> nil then
      DropDownMenuOwner := SelectedNodeViewItem;
  finally
    FLockDropDownMenuWindowDestroying := False;
  end;
end;

procedure TdxBreadcrumbEditController.ShowNodeDropDownMenu(ANodeViewItem: TdxBreadcrumbEditNodeViewItem);
begin
  if ANodeViewItem <> nil then
  begin
    DoNodeDropDownPopup(ANodeViewItem.Node);
    if not ViewInfo.IsViewItemValid(ANodeViewItem) then
      ANodeViewItem := nil;
  end;

  DropDownMenuOwner := ANodeViewItem;
  if DropDownMenuOwner <> nil then
  begin
    dxMessagesController.KillMessages(Handle, DXM_BREADCRUMBEDIT_CLOSEPOPUPWINDOW);
    if FDropDownMenuWindow = nil then
    begin
      FDropDownMenuWindow := CreateDropDownMenuWindow;
      FDropDownMenuWindow.ShowHint := Control.GetShowHint;
      FDropDownMenuWindow.ModalMode := False;
    end;
    FDropDownMenuWindow.Popup;
  end;
end;

procedure TdxBreadcrumbEditController.ValidateItem(var AItem: TdxBreadcrumbEditViewItem);
begin
  if not ViewInfo.IsViewItemValid(AItem) then
    AItem := nil;
end;

procedure TdxBreadcrumbEditController.ValidateItems;
begin
  ValidateItem(TdxBreadcrumbEditViewItem(FDropDownMenuOwner));
  ValidateItem(FFocusedViewItem);
  ValidateItem(FPressedViewItem);
end;

procedure TdxBreadcrumbEditController.ValidatePath(
  const APath: string; var ANode: TdxBreadcrumbEditNode);
var
  AError: Boolean;
  AErrorText: string;
begin
  AError := ANode = nil;
  if AError then
    AErrorText := Format(cxGetResourceString(@sdxBreadcrumbEditInvalidPath), [APath]);
  DoValidatePath(APath, ANode, AErrorText, AError);
  if AError then
    raise EdxBreadcrumbEditValidationError.Create(AErrorText);
end;

procedure TdxBreadcrumbEditController.UpdateHitTest;
begin
  if ControlContainer.HandleAllocated then
  begin
    ViewInfo.CalculateHitTest(ControlContainer.ScreenToClient(GetMouseCursorPos));
    MouseInControl := PtInRect(ViewInfo.Bounds, HitTestInfo.Point);
    FocusedViewItem := HitTestInfo.ViewItem;
  end
  else
    MouseInControl := False;
end;

procedure TdxBreadcrumbEditController.UpdateState;
begin
  if not Destroying then
    ViewInfo.State := CalculateState;
end;

procedure TdxBreadcrumbEditController.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    DXM_BREADCRUMBEDIT_RELEASEPATHEDITOR:
      PathEditingController.ClosePathEditor;
    DXM_BREADCRUMBEDIT_CLOSEPOPUPWINDOW:
      begin
        if IsFocused and (Message.WParam <> 0) then
          FocusedViewItem := DropDownMenuOwner
        else
          FocusedViewItem := nil;

        HideNodeDropDownMenu;
      end;
  end;
  inherited WndProc(Message);
end;

function TdxBreadcrumbEditController.GetControl: IdxBreadcrumbEdit;
begin
  Result := ViewInfo.Control;
end;

function TdxBreadcrumbEditController.GetControlContainer: TWinControl;
begin
  Result := Control.GetContainer;
end;

function TdxBreadcrumbEditController.GetHitTestInfo: TdxBreadcrumbEditHitTestInfo;
begin
  Result := ViewInfo.HitTestInfo;
end;

function TdxBreadcrumbEditController.GetIsFocused: Boolean;
begin
  Result := Control.GetIsFocused or PathEditingController.IsFocused;
end;

function TdxBreadcrumbEditController.GetIsUpdateLocked: Boolean;
begin
  Result := FUpdateLockCount > 0;
end;

function TdxBreadcrumbEditController.GetRoot: TdxBreadcrumbEditNode;
begin
  Result := Control.GetRoot;
end;

function TdxBreadcrumbEditController.GetSelectedPath: string;
begin
  if Selected <> nil then
    Result := Selected.Path
  else
    Result := '';
end;

function TdxBreadcrumbEditController.GetSelectedNodeViewItem: TdxBreadcrumbEditNodeViewItem;
begin
  Result := ViewInfo.NodesAreaViewInfo.SelectedNodeViewItem;
end;

procedure TdxBreadcrumbEditController.SetDropDownMenuOwner(AValue: TdxBreadcrumbEditNodeViewItem);
begin
  FDropDownMenuOwner := AValue;
  ViewInfo.UpdateViewItemStates;
end;

procedure TdxBreadcrumbEditController.SetFocusedViewItem(AValue: TdxBreadcrumbEditViewItem);
begin
  FFocusedViewItem := AValue;
  ViewInfo.UpdateViewItemStates;
end;

procedure TdxBreadcrumbEditController.SetMouseInControl(AValue: Boolean);
begin
  if AValue <> FMouseInControl then
  begin
    FMouseInControl := AValue;
    UpdateState;
  end;
end;

procedure TdxBreadcrumbEditController.SetPressedViewItem(AValue: TdxBreadcrumbEditViewItem);
begin
  FPressedViewItem := AValue;
  ViewInfo.UpdateViewItemStates;
end;

procedure TdxBreadcrumbEditController.SetSelected(AValue: TdxBreadcrumbEditNode);
begin
  if CanSelectNode(AValue) then
  begin
    if AValue <> FSelected then
    begin
      DoBeforeSelect;
      FSelected := AValue;
      DoAfterSelect;
      Changed([bcecNodes, bcecLayout, bcecSelection]);
    end;
  end;
end;

{ TdxCustomBreadcrumbEdit }

constructor TdxCustomBreadcrumbEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csActionClient];
  FRoot := CreateRoot;
  FProperties := CreateProperties;
  FProperties.OnChange := PropertiesChanged;
  FViewInfo := CreateViewInfo;
  FController := CreateController;
  FocusOnClick := True;
  Keys := [kArrows];
  TabStop := True;
  Color := clDefault;
  AutoSize := True;
  DoubleBuffered := True;
  FRoot.Name := sdxBreadcrumbEditDefaultRootName;
  SetBounds(Left, Top, 200, dxBreadcrumbEditDefaultHeight);
end;

destructor TdxCustomBreadcrumbEdit.Destroy;
begin
  FProperties.OnChange := nil;
  FreeAndNil(FRoot);
  FreeAndNil(FController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TdxCustomBreadcrumbEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  if Selected = nil then
    Selected := Root;
end;

procedure TdxCustomBreadcrumbEdit.AdjustAutoSize;
begin
  if AutoSize then
    AdjustSize;
end;

procedure TdxCustomBreadcrumbEdit.BeforeDelete(ASender: TdxTreeCustomNode);
begin
  if Assigned(OnDeleteNode) then
    OnDeleteNode(Self, ASender as TdxBreadcrumbEditNode);
end;

function TdxCustomBreadcrumbEdit.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  NewHeight := ViewInfo.CalculateAutoHeight;
  Result := True;
end;

function TdxCustomBreadcrumbEdit.CanCollapse(ASender: TdxTreeCustomNode): Boolean;
begin
  Result := True;
end;

function TdxCustomBreadcrumbEdit.CanExpand(ASender: TdxTreeCustomNode): Boolean;
begin
  Result := True;
end;

procedure TdxCustomBreadcrumbEdit.Collapsed(ASender: TdxTreeCustomNode);
begin
  //nothing
end;

procedure TdxCustomBreadcrumbEdit.Changed(AChanges: TdxBreadcrumbEditChanges);
begin
  Controller.Changed(AChanges);
end;

procedure TdxCustomBreadcrumbEdit.ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Properties.ChangeScale(M, D);
end;

procedure TdxCustomBreadcrumbEdit.CreateWnd;
begin
  inherited CreateWnd;
  RecalculateViewInfo;
  AdjustAutoSize;
end;

function TdxCustomBreadcrumbEdit.CreateController: TdxBreadcrumbEditController;
begin
  Result := TdxBreadcrumbEditController.Create(ViewInfo);
end;

function TdxCustomBreadcrumbEdit.CreateProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := TdxCustomBreadcrumbEditProperties.Create(Self);
end;

function TdxCustomBreadcrumbEdit.CreateViewInfo: TdxBreadcrumbEditViewInfo;
begin
  Result := TdxBreadcrumbEditViewInfo.Create(Self);
end;

procedure TdxCustomBreadcrumbEdit.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if Transparent then
    cxDrawTransparentControlBackground(Self, ACanvas, R)
  else
    ACanvas.FillRect(R, BackgroundColor);
end;

procedure TdxCustomBreadcrumbEdit.DeleteNode(ASender: TdxTreeCustomNode);
begin
  if not IsDestroying then
  begin
    Controller.DoDeleteNode(ASender as TdxBreadcrumbEditNode);
    Changed([bcecNodes, bcecLayout]);
  end;
end;

procedure TdxCustomBreadcrumbEdit.Expanded(ASender: TdxTreeCustomNode);
begin
  //nothing
end;

function TdxCustomBreadcrumbEdit.GetTreeOwner: TPersistent;
begin
  Result := Self;
end;

function TdxCustomBreadcrumbEdit.GetNodeClass(ARelativeNode: TdxTreeCustomNode): TdxTreeCustomNodeClass;
begin
  Result := TdxTreeCustomNodeClass(Root.ClassType);
end;

procedure TdxCustomBreadcrumbEdit.LoadChildren(ASender: TdxTreeCustomNode);
begin
  if Assigned(OnPopulateChildren) then
    OnPopulateChildren(Self, ASender as TdxBreadcrumbEditNode);
end;

procedure TdxCustomBreadcrumbEdit.TreeNotification(
  ASender: TdxTreeCustomNode; ANotification: TdxTreeNodeNotifications);
const
  NotificationsMap: array[TdxTreeNodeNotification] of TdxBreadcrumbEditChanges =
    ([bcecNodes, bcecLayout], [bcecLayout]);
var
  AChanges: TdxBreadcrumbEditChanges;
  ATreeNotification: TdxTreeNodeNotification;
begin
  AChanges := [];
  for ATreeNotification := Low(ATreeNotification) to High(ATreeNotification) do
  begin
    if ATreeNotification in ANotification then
      AChanges := AChanges + NotificationsMap[ATreeNotification];
  end;
  Changed(AChanges);
end;

procedure TdxCustomBreadcrumbEdit.BeginUpdate;
begin
  Controller.BeginUpdate;
end;

procedure TdxCustomBreadcrumbEdit.EndUpdate;
begin
  Controller.EndUpdate;
end;

procedure TdxCustomBreadcrumbEdit.BrowseParent;
begin
  if (Selected <> nil) and (Selected.Parent <> nil) then
    Selected := Selected.Parent;
end;

procedure TdxCustomBreadcrumbEdit.FocusChanged;
begin
  inherited FocusChanged;
  Controller.FocusChanged;
end;

procedure TdxCustomBreadcrumbEdit.FontChanged;
begin
  inherited FontChanged;
  Changed([bcecLayout]);
end;

procedure TdxCustomBreadcrumbEdit.InitiateAction;
begin
  inherited InitiateAction;
  Properties.InitiateActions;
end;

procedure TdxCustomBreadcrumbEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Controller.KeyDown(Key, Shift);
end;

procedure TdxCustomBreadcrumbEdit.Loaded;
begin
  inherited Loaded;
  RecalculateViewInfo;
end;

procedure TdxCustomBreadcrumbEdit.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  RecalculateViewInfo;
  Changed([bcecAnimation, bcecLayout]);
end;

procedure TdxCustomBreadcrumbEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Point(X, Y), Shift);
end;

procedure TdxCustomBreadcrumbEdit.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Controller.MouseLeave;
end;

procedure TdxCustomBreadcrumbEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Controller.MouseMove(Point(X, Y), Shift);
end;

procedure TdxCustomBreadcrumbEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Button, Point(X, Y), Shift);
end;

procedure TdxCustomBreadcrumbEdit.NodeDropDownPopup(ANode: TdxBreadcrumbEditNode);
begin
  if Assigned(OnNodeDropDownPopup) then
    OnNodeDropDownPopup(Self, ANode);
end;

procedure TdxCustomBreadcrumbEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Properties <> nil then
    Properties.Notification(AComponent, Operation);
end;

procedure TdxCustomBreadcrumbEdit.Paint;
begin
  DrawBackground(Canvas, ClientRect);
  ViewInfo.Draw(Canvas);
end;

procedure TdxCustomBreadcrumbEdit.PropertiesChanged(
  Sender: TObject; AChanges: TdxBreadcrumbEditChanges);
begin
  Changed(AChanges);
end;

procedure TdxCustomBreadcrumbEdit.PathEntered(var ANewPath: string; var AHandled: Boolean);
begin
  if Assigned(OnPathEntered) then
    OnPathEntered(Self, ANewPath, AHandled);
end;

procedure TdxCustomBreadcrumbEdit.PathValidate(const APath: string;
  var ANode: TdxBreadcrumbEditNode; var AErrorText: string; var AError: Boolean);
begin
  if Assigned(OnPathValidate) then
    OnPathValidate(Self, APath, ANode, AErrorText, AError);
end;

procedure TdxCustomBreadcrumbEdit.PopulateAutoCompleteSuggestions(
  const APath: string; ASuggestions: TStringList);
begin
  if Assigned(OnPopulateAutoCompleteSuggestions) then
    OnPopulateAutoCompleteSuggestions(Self, APath, ASuggestions);
end;

procedure TdxCustomBreadcrumbEdit.ProcessButtonClick(AButton: TdxBreadcrumbEditButton);
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Self, AButton);
end;

procedure TdxCustomBreadcrumbEdit.SelectionChanged;
begin
  if Assigned(OnPathSelected) then OnPathSelected(Self);
end;

procedure TdxCustomBreadcrumbEdit.RecalculateViewInfo;
begin
  if Controller <> nil then
    Controller.BoundsChanged(ClientBounds);
end;

procedure TdxCustomBreadcrumbEdit.Resize;
begin
  inherited Resize;
  RecalculateViewInfo;
end;

procedure TdxCustomBreadcrumbEdit.CMHintShow(var Message: TCMHintShow);
begin
  ViewInfo.CalculateHitTest(Message.HintInfo^.CursorPos);
  if ViewInfo.HitTestInfo.ViewItem <> nil then
  begin
    Message.HintInfo^.HintStr := GetShortHint(ViewInfo.HitTestInfo.ViewItem.HintText);
    Message.HintInfo^.CursorRect := ViewInfo.HitTestInfo.ViewItem.Bounds;
  end
  else
    Message.Result := -1;
end;

procedure TdxCustomBreadcrumbEdit.EnabledChanged;
begin
  Controller.UpdateState;
end;

function TdxCustomBreadcrumbEdit.GetBackgroundColor: TColor;
begin
  Result := Color;
  if Result = clDefault then
    Result := LookAndFeelPainter.BreadcrumbEditBackgroundColor(ViewInfo.State);
end;

function TdxCustomBreadcrumbEdit.GetContainer: TWinControl;
begin
  Result := Self;
end;

function TdxCustomBreadcrumbEdit.GetController: TdxBreadcrumbEditController;
begin
  Result := FController;
end;

function TdxCustomBreadcrumbEdit.GetFont: TFont;
begin
  Result := Font;
end;

function TdxCustomBreadcrumbEdit.GetIsEnabled: Boolean;
begin
  Result := Enabled;
end;

{$IFNDEF VCLGLASSPAINT}
function TdxCustomBreadcrumbEdit.GetOnGlass: Boolean;
begin
  Result := OnGlass;
end;
{$ENDIF}

function TdxCustomBreadcrumbEdit.GetPathDelimiter: Char;
begin
  Result := Properties.PathEditor.PathDelimiter;
end;

function TdxCustomBreadcrumbEdit.GetProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := FProperties;
end;

function TdxCustomBreadcrumbEdit.GetRoot: TdxBreadcrumbEditNode;
begin
  Result := FRoot;
end;

function TdxCustomBreadcrumbEdit.GetSelected: TdxBreadcrumbEditNode;
begin
  Result := Controller.Selected;
end;

function TdxCustomBreadcrumbEdit.GetSelectedPath: string;
begin
  Result := Controller.SelectedPath;
end;

function TdxCustomBreadcrumbEdit.GetShowHint: Boolean;
begin
  Result := ShowHint;
end;

procedure TdxCustomBreadcrumbEdit.SetProperties(AValue: TdxCustomBreadcrumbEditProperties);
begin
  FProperties.Assign(AValue);
end;

procedure TdxCustomBreadcrumbEdit.SetSelected(AValue: TdxBreadcrumbEditNode);
begin
  Controller.SelectPath(AValue);
end;

procedure TdxCustomBreadcrumbEdit.SetSelectedPath(const AValue: string);
begin
  Controller.SelectPath(AValue);
end;

{ TdxBreadcrumbEdit }

function TdxBreadcrumbEdit.CreateProperties: TdxCustomBreadcrumbEditProperties;
begin
  Result := TdxBreadcrumbEditProperties.Create(Self);
end;

function TdxBreadcrumbEdit.CreateRoot: TdxBreadcrumbEditNode;
begin
  Result := TdxBreadcrumbEditNode.Create(Self);
end;

procedure TdxBreadcrumbEdit.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadOldNodeData, nil, False);
  Filer.DefineBinaryProperty('NodeData', ReadNodeData, WriteNodeData, True);
end;

function TdxBreadcrumbEdit.IsSelectedPathStored: Boolean;
begin
  Result := Selected <> Root;
end;

procedure TdxBreadcrumbEdit.Loaded;
begin
  inherited Loaded;
  if FLoadedSelectedPath <> '' then
    SelectedPath := FLoadedSelectedPath;
end;

procedure TdxBreadcrumbEdit.ReadNodeData(AStream: TStream);
var
  AVersion: Cardinal;
begin
  BeginUpdate;
  try
    AStream.ReadBuffer(AVersion, SizeOf(AVersion));
    if AVersion <> dxBreadcrumbEditStreamVersion then
      raise EdxBreadcrumbEditValidationError.CreateFmt(cxGetResourceString(@sdxBreadcrumbEditInvalidStreamVersion), [AVersion]);
    Root.ReadData(AStream, AVersion);
  finally
    EndUpdate;
  end;
end;

procedure TdxBreadcrumbEdit.ReadOldNodeData(AStream: TStream);
begin
  BeginUpdate;
  try
    Root.ReadData(AStream);
  finally
    EndUpdate;
  end;
end;

procedure TdxBreadcrumbEdit.WriteNodeData(AStream: TStream);
var
  AVersion: Cardinal;
begin
  BeginUpdate;
  try
    AVersion := dxBreadcrumbEditStreamVersion;
    AStream.WriteBuffer(AVersion, SizeOf(AVersion));
    Root.WriteData(AStream);
  finally
    EndUpdate;
  end;
end;

function TdxBreadcrumbEdit.GetSelectedPath: string;
begin
  if IsLoading then
    Result := FLoadedSelectedPath
  else
    Result := inherited GetSelectedPath;
end;

function TdxBreadcrumbEdit.GetProperties: TdxBreadcrumbEditProperties;
begin
  Result := inherited Properties as TdxBreadcrumbEditProperties;
end;

procedure TdxBreadcrumbEdit.SetProperties(AValue: TdxBreadcrumbEditProperties);
begin
  inherited Properties := AValue;
end;

procedure TdxBreadcrumbEdit.SetSelectedPath(const AValue: string);
begin
  if IsLoading then
    FLoadedSelectedPath := AValue
  else
    inherited SetSelectedPath(AValue);
end;

end.
