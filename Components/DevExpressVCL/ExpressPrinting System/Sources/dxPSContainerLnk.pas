{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSContainerLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls,
  TabNotBk, Forms, ImgList, Menus, dxBase, dxPSCore, dxPSSngltn, dxPSShapes, dxPSForm,
  dxPSFillPatterns, dxPSStandardFillPatterns, Buttons, cxDrawTextUtils, cxPC, cxControls,
  cxContainer, cxEdit, cxCheckBox, cxListBox, cxLabel, cxLookAndFeelPainters, cxButtons,
  cxGraphics, cxTreeView, cxGroupBox, dxPSReportRenderCanvas, cxGeometry, cxLookAndFeels,
  cxScrollBox, dxPSReportLinkDesignWindow, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutcxEditAdapters, cxImageList, cxImage;

type
  TdxCustomContainerReportLink = class;
  TdxCustomContainerReportLinkOptionsItemPlaceClass = class of TdxCustomContainerReportLinkOptionsItemPlace;
  TdxCustomContainerReportLinkOptionsItemPlace = class;
  TdxCustomContainerReportLinkOptionsPaginationClass = class of TdxCustomContainerReportLinkOptionsPagination;
  TdxCustomContainerReportLinkOptionsPagination = class;
  TdxCustomContainerReportLinkOptionsTransparentClass = class of TdxCustomContainerReportLinkOptionsTransparent;
  TdxCustomContainerReportLinkOptionsTransparent = class;
  TdxfmCustomContainerDesignWindow = class;

  { Report Items }

  TdxReportWinControlHost = class(TdxReportCell)
  private
    function GetHasControlItem: Boolean;
  protected
    procedure BoundsChanged; override;
    function GetControlItem: TdxReportVisualItem; virtual;
  public
    function MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;

    property ControlItem: TdxReportVisualItem read GetControlItem;
    property HasControlItem: Boolean read GetHasControlItem;
  end;

  TdxReportNativePrintableControlHost = class(TdxReportWinControlHost)
  protected
    function GetControlItem: TdxReportVisualItem; override;
    procedure BoundsChanged; override;
  end;

  { Definitions }

  TdxPSCustomContainerItemDefinitionClass = class of TdxPSCustomContainerItemDefinition;

  TdxPSCustomContainerItemDefinition = class(TPersistent)
  private
    FComponent: TComponent;
    FOptionsPlace: TdxCustomContainerReportLinkOptionsItemPlace;
    FReportItem: TdxReportVisualItem;
    FReportLink: TdxCustomContainerReportLink;
    function GetRootContainer: TWinControl;
    function GetTopLevelRootContainer: TWinControl;
  protected
    procedure AddDelimitersHorz(AList: TList); virtual;
    procedure AddDelimitersVert(AList: TList); virtual;
    procedure AddReportItemToDelimitersHorz(AList: TList);
    procedure AddReportItemToDelimitersVert(AList: TList);

    function GetSizeChangeReportItem: TdxReportVisualItem; virtual;
    function GetSizeMeasureReportItem: TdxReportVisualItem; virtual;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink; AComponent: TComponent; AReportItem: TdxReportVisualItem); virtual;
    destructor Destroy; override;

    function OptionsPagination: TdxCustomContainerReportLinkOptionsPagination; overload; virtual;

    property Component: TComponent read FComponent write FComponent;
    property OptionsPlace: TdxCustomContainerReportLinkOptionsItemPlace read FOptionsPlace;
    property ReportItem: TdxReportVisualItem read FReportItem write FReportItem;
    property ReportLink: TdxCustomContainerReportLink read FReportLink;
    property RootContainer: TWinControl read GetRootContainer;
    property TopLevelRootContainer: TWinControl read GetTopLevelRootContainer;
    property SizeChangeReportItem: TdxReportVisualItem read GetSizeChangeReportItem;
    property SizeMeasureReportItem: TdxReportVisualItem read GetSizeMeasureReportItem;
  end;

  TdxPSContainerControlDefinition = class(TdxPSCustomContainerItemDefinition)
  private
    function GetControl: TControl;
    procedure SetControl(Value: TControl);
  protected
    procedure AddDelimitersHorz(AList: TList); override;
    procedure AddDelimitersVert(AList: TList); override;
  public
    property Control: TControl read GetControl write SetControl;
  end;

  TdxPSNativePrintableControlDefinition = class(TdxPSContainerControlDefinition)
  private
    FDelimitersHorz: TList;
    FDelimitersVert: TList;
    FReportDimension: TPoint;
    function GetDelimitersOffset: TPoint;
  protected
    procedure AddDelimitersHorz(AList: TList); override;
    procedure AddDelimitersVert(AList: TList); override;
    procedure GetData(AReportLink: TBasedxReportLink);
    procedure ShiftDelimiters;

    function GetSizeChangeReportItem: TdxReportVisualItem; override;

    property DelimitersOffset: Tpoint read GetDelimitersOffset;
    property DelimitersHorz: TList read FDelimitersHorz;
    property DelimitersVert: TList read FDelimitersVert;
    property ReportDimension: TPoint read FReportDimension;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink; AComponent: TComponent; AReportItem: TdxReportVisualItem); override;
    destructor Destroy; override;
  end;

  TdxPSContainerWinControlDefinition = class(TdxPSContainerControlDefinition)
  protected
    function GetSizeChangeReportItem: TdxReportVisualItem; override;
  end;

  TdxPSContainerDefinition = class(TdxPSContainerWinControlDefinition)
  protected
    function GetSizeMeasureReportItem: TdxReportVisualItem; override;
  end;

  { Iterators }

  IdxPSContainerIterator = interface
  ['{B18A68C0-5505-42AC-9B8D-B96C79A2725E}']
    function GetControl(Index: Integer): TControl;
    function GetControlCount: Integer;
    procedure GoBeforeBOF;
    procedure GoBeyondEOF;
    function IsBOF: Boolean;
    function IsEOF: Boolean;
    function Next: TControl;
    function Prev: TControl;

    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TControl read GetControl;
  end;

  TdxPSWinControlIteratorClass = class of TdxPSWinControlIterator;

  TdxPSWinControlIterator = class(TInterfacedObject, IdxPSContainerIterator)
  private
    FControl: TWinControl;
    FCounter: Integer;
  protected
    { IdxPSContainerIterator }
    function GetControl(Index: Integer): TControl; virtual;
    function GetControlCount: Integer; virtual;
    procedure GoBeforeBOF; virtual;
    procedure GoBeyondEOF; virtual;
    function IsBOF: Boolean; virtual;
    function IsEOF: Boolean; virtual;
    function Next: TControl; virtual;
    function Prev: TControl; virtual;

    property ControlCount: Integer read GetControlCount;
    property Controls[Index: Integer]: TControl read GetControl;
  public
    constructor Create(AControl: TWinControl); virtual;
    class function ContainerClass: TWinControlClass; virtual;
    property Control: TWinControl read FControl;
  end;

  { Producers }

  TdxPSCustomProducerClass = class of TdxPSCustomProducer;

  TdxPSCustomProducer = class(TdxCustomClassMapItem)
  private
    FProducingObject: TComponent;
    FReportLink: TdxCustomContainerReportLink;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetDefinition(Component: TComponent): TdxPSCustomContainerItemDefinition;
    function GetIsDesigning: Boolean;
    function GetProducer(Component: TComponent): TdxPSCustomProducer;
  protected
    class function BuddyClass: TdxPSCustomProducerClass; virtual;
    class function HelperProducer: TdxPSCustomProducerClass; virtual;

    function CanProcessChild(AChildControl: TControl): Boolean; virtual;

    function GetScrollPosLeft: Integer; virtual;
    function GetScrollPosTop: Integer; virtual;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); virtual;
    procedure InitializeOptionsPlace(AnOptions: TdxCustomContainerReportLinkOptionsItemPlace); virtual;

    procedure DoReposition; virtual;
    function MeasureItemHeight(AItem: TdxReportVisualItem): Integer; virtual;
    function MeasureItemWidth(AItem: TdxReportVisualItem): Integer; virtual;
    function ObjectExpandHeight: Boolean; virtual;
    function ObjectExpandWidth: Boolean; virtual;
    function ObjectShrinkHeight: Boolean; virtual;
    function ObjectShrinkWidth: Boolean; virtual;

    function OptionsTransparent: TdxCustomContainerReportLinkOptionsTransparent; overload; virtual;

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property Definitions[Component: TComponent]: TdxPSCustomContainerItemDefinition read GetDefinition;
    property IsDesigning: Boolean read GetIsDesigning;
    property Producers[Component: TComponent]: TdxPSCustomProducer read GetProducer;
    property ScrollPosLeft: Integer read GetScrollPosLeft;
    property ScrollPosTop: Integer read GetScrollPosTop;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink; AnObject: TComponent); virtual;

    class function CanHasAvailableChildren: Boolean; virtual;
    class function HasNativeSupportForBorders: Boolean; virtual;
    class function Reenterable: Boolean; virtual;

    function Definition: TdxPSCustomContainerItemDefinition; overload; virtual;
    class function DefinitionClass: TdxPSCustomContainerItemDefinitionClass; virtual; // must be overriden

    procedure Initialize(AnObject: TComponent); virtual;

    function ProducingObject: TComponent; overload; virtual;
    function ProducingObjectFriendlyName: string; virtual;

    function ReportLink: TdxCustomContainerReportLink; overload; virtual;
    procedure Reposition; virtual;
    function RootContainer: TWinControl; overload; virtual;
    function TopLevelRootContainer: TWinControl; overload; virtual;

    class procedure Register; virtual;
    class procedure Unregister; virtual;
  end;

  TdxPSCustomContainerItemProducerClass = class of TdxPSCustomContainerItemProducer;

  TdxPSCustomContainerItemProducer = class(TdxPSCustomProducer)
  private
    function GetControlBounds: TRect;
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    function CreateIterator: IdxPSContainerIterator; virtual;

    function GetContentColor: TColor; virtual;
    function GetControlBoundsRect: TRect; virtual;
    function GetFont: TFont; virtual;
    function GetFontColor: TColor; virtual;
    function GetFontIndex: Integer; virtual;
    function GetFontName: string; virtual;
    function GetFontOrientation: Integer; virtual;
    function GetFontStyle: TFontStyles; virtual;
    function IsFontSubstitutable: Boolean; virtual;

    function HostClass: TdxReportCellClass; virtual;
    procedure InitializeHost(ACell: TdxReportCell); virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); virtual;
    function ItemClass: TdxReportVisualItemClass; virtual;
  public
    function Control: TControl; overload; virtual;
    class function ControlClass: TControlClass; virtual;

    function Definition: TdxPSContainerControlDefinition; reintroduce; overload;
    class function DefinitionClass: TdxPSCustomContainerItemDefinitionClass; override;

    class function PairClass: TClass; override;

    function ProducingObject: TControl; reintroduce; overload;
    function ProducingObjectFriendlyName: string; override;

    property ContentColor: TColor read GetContentColor;
    property ControlBounds: TRect read GetControlBounds;
    property Font: TFont read GetFont;
    property FontColor: TColor read GetFontColor;
    property FontIndex: Integer read GetFontIndex;
    property FontName: string read GetFontName;
    property FontOrientation: Integer read GetFontOrientation;
    property FontStyle: TFontStyles read GetFontStyle;
  end;

  TdxPSCustomDelegateProducer = class(TdxPSCustomContainerItemProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function Producer: TdxPSCustomContainerItemProducer; virtual; abstract;
  end;

  TdxPSNativePrintableControlProducerClass = class of TdxPSNativePrintableControlProducer;

  TdxPSNativePrintableControlProducer = class(TdxPSCustomContainerItemProducer)
  private
    FIsOuterLinkUsed: Boolean;
  protected
    procedure AdjustItemBounds(AnItem: TdxReportVisualItem); virtual;

    function CreateControlReportLink(var AIsOuterLinkUsed: Boolean): TBasedxReportLink;
    procedure CreateNativePrintableControlData(AItem: TdxReportVisualItem);
    procedure DeinitializeReportLink(AControlReportLink: TBasedxReportLink); virtual;
    class function GetLinkClass(AClass: TClass): TdxReportLinkClass;
    class function HasReportLink(AComponent: TComponent): Boolean;
    procedure InitializeReportLink(AControlReportLink: TBasedxReportLink); virtual;

    function HostClass: TdxReportCellClass; override;
    procedure InitializeHost(ACell: TdxReportCell); override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeNativePrintableControlHost(AnItem: TdxReportVisualItem); virtual;
    function ItemClass: TdxReportVisualItemClass; override;

    function MeasureItemHeight(AnItem: TdxReportVisualItem): Integer; override;
    function MeasureItemWidth(AnItem: TdxReportVisualItem): Integer; override;
    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
    //
    property IsOuterLinkUsed: Boolean read FIsOuterLinkUsed;
  public
    class function Reenterable: Boolean; override;
    function Definition: TdxPSNativePrintableControlDefinition; reintroduce; overload;
    class function DefinitionClass: TdxPSCustomContainerItemDefinitionClass; override;

    class function HasNativeSupportForBorders: Boolean; override;

    class procedure Register; override;
    class procedure Unregister; override;
  end;

  TdxPSContainerControlProducer = class(TdxPSCustomContainerItemProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  end;

  TdxPSControlAsMetafileProducer = class(TdxPSContainerControlProducer)
  protected
    class function BuddyClass: TdxPSCustomProducerClass; override;
    function CreateControlImage(AItem: TdxReportVisualItem): TGraphic; virtual;
    procedure InitializeItem(AItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    class function PairClass: TClass; override;
    class procedure Register; override;
    class procedure Unregister; override;

    procedure Reposition; override;
  end;

  TdxPSBevelProducer = class(TdxPSContainerControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TBevel; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSPaintBoxProducer = class(TdxPSContainerControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TPaintBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSShapeProducer = class(TdxPSContainerControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TShape; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomLabelProducer = class(TdxPSContainerControlProducer)
  protected
    function GetControlBoundsRect: TRect; override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
  public
    function Control: TCustomLabel; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSContainerCustomWinControlProducer = class(TdxPSContainerControlProducer)
  protected
    function CreateIterator: IdxPSContainerIterator; override;
    function HostClass: TdxReportCellClass; override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    class function IteratorClass: TdxPSWinControlIteratorClass; virtual;
  public
    function Control: TWinControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    class function DefinitionClass: TdxPSCustomContainerItemDefinitionClass; override;
  end;

  TdxPSWinControlAsMetafileProducer = class(TdxPSContainerCustomWinControlProducer)
  protected
    class function BuddyClass: TdxPSCustomProducerClass; override;
    function CreateControlImage(AItem: TdxReportVisualItem): TGraphic; virtual;
    procedure InitializeItem(AItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    class function PairClass: TClass; override;
    procedure Reposition; override;
    class procedure Register; override;
    class procedure Unregister; override;
  end;

  TdxPSContainerWinControlProducer = class(TdxPSContainerCustomWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectExpandHeight: Boolean; override;
  end;

  TdxPSDateTimePickerProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TDateTimePicker; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomHotKeyProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TCustomHotKey; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomStaticTextProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TCustomStaticText; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomEditProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TCustomEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomMemoProducer = class(TdxPSCustomEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ObjectExpandHeight: Boolean; override;
  public
    function Control: TCustomMemo; reintroduce; overload;
    class function ControlClass: TControlClass; override;

    function ProducingObjectFriendlyName: string; override;
  end;

  TdxPSCustomComboBoxProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TCustomComboBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomComboBoxExProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TCustomComboBoxEx; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomCheckBoxProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TCustomCheckBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSRadioButtonProducer = class(TdxPSContainerWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TRadioButton; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomContainerProducerClass = class of TdxPSCustomContainerProducer;

  TdxPSCustomContainerProducer = class(TdxPSContainerCustomWinControlProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
    function ObjectShrinkHeight: Boolean; override;
    function ObjectShrinkWidth: Boolean; override;
  public
    class function CanHasAvailableChildren: Boolean; override;
    class function DefinitionClass: TdxPSCustomContainerItemDefinitionClass; override;
  end;

  TdxPSCustomPanelProducer = class(TdxPSCustomContainerProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
  public
    function Control: TCustomPanel; reintroduce; overload;
    class function ControlClass: TControlClass; override;

    class function HasNativeSupportForBorders: Boolean; override;
  end;

  TdxPSCustomGroupBoxProducer = class(TdxPSCustomContainerProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeLookAndFeel(AnItem: TdxReportVisualItem; ALookAndFeel: TdxPSReportGroupLookAndFeel);
    function ItemClass: TdxReportVisualItemClass; override;
    class function LookAndFeelClass: TdxPSReportGroupLookAndFeelClass; virtual;
  public
    function Control: TCustomGroupBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;

    class function HasNativeSupportForBorders: Boolean; override;
  end;

  TdxPSCustomRadioGroupProducer = class(TdxPSCustomGroupBoxProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    procedure CreateItems(AReportRadioGroup: TdxReportRadioGroup);
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeRadioItem(AnItem: TdxCustomReportCellRadio; AnIndex: Integer); virtual;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TCustomRadioGroup; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSRootContainerProducer = class(TdxPSCustomContainerProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  end;

  TdxPSTabControlProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TTabControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSPageControlIterator = class(TdxPSWinControlIterator)
  private
    function GetPageControl: TPageControl;
  protected
    function GetControl(Index: Integer): TControl; override;
    function GetControlCount: Integer; override;
  public
    property PageControl: TPageControl read GetPageControl;
  end;

  TdxPSTabSheetProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TTabSheet; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSPageControlProducer = class(TdxPSRootContainerProducer)//TdxPSCustomDelegateProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    class function IteratorClass: TdxPSWinControlIteratorClass; override;
  public
    function Control: TPageControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSNotebookIterator = class(TdxPSWinControlIterator)
  private
    function GetNotebook: TNotebook;
  protected
    function GetControl(Index: Integer): TControl; override;
    function GetControlCount: Integer; override;
  public
    property Notebook: TNotebook read GetNotebook;
  end;

  TdxPSNotebookPageProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TPage; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSNotebookProducer = class(TdxPSRootContainerProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    class function IteratorClass: TdxPSWinControlIteratorClass; override;
  public
    function Control: TNoteBook; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSTabbedNotebookIterator = class(TdxPSWinControlIterator)
  private
    function GetTabbedNotebook: TTabbedNotebook;
  protected
    function GetControl(Index: Integer): TControl; override;
    function GetControlCount: Integer; override;
  public
    property TabbedNotebook: TTabbedNotebook read GetTabbedNotebook;
  end;

  TdxPSTabbedNotebookPageProducer = class(TdxPSRootContainerProducer)
  public
    function Control: TTabPage; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSTabbedNotebookProducer = class(TdxPSRootContainerProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    class function IteratorClass: TdxPSWinControlIteratorClass; override;
  public
    function Control: TTabbedNotebook; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSScrollingWinControlProducer = class(TdxPSRootContainerProducer)
  protected
    function GetScrollBarPos(AScrollBar: TControlScrollBar): Integer; virtual;
    function GetScrollPosLeft: Integer; override;
    function GetScrollPosTop: Integer; override;

    procedure InitializeHost(ACell: TdxReportCell); override;

    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
  public
    function Control: TScrollingWinControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPScxScrollBoxProducer = class(TdxPSRootContainerProducer)
  protected
    function GetScrollBarPos(AScrollBar: TcxScrollBoxScrollBarOptions): Integer; virtual;
    function GetScrollPosLeft: Integer; override;
    function GetScrollPosTop: Integer; override;

    procedure InitializeHost(ACell: TdxReportCell); override;

    function ObjectExpandHeight: Boolean; override;
    function ObjectExpandWidth: Boolean; override;
  public
    function Control: TcxCustomScrollBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomFrameProducer = class(TdxPSScrollingWinControlProducer)
  public
    function Control: TCustomFrame; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  TdxPSCustomFormProducer = class(TdxPSScrollingWinControlProducer)
  public
    function Control: TCustomForm; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { container builder }

  TdxPSContainerBuilderClass = class of TdxPSContainerBuilder;

  TdxPSContainerBuilder = class(TdxCustomClassMapItem)
  private
    FContainer: TWinControl;
    FCurrentControl: TControl;
    FHost: TdxReportCell;
    FParentBuilder: TdxPSContainerBuilder;
    FParentHost: TdxReportCell;
    FReportLink: TdxCustomContainerReportLink;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetIsRoot: Boolean;
    function GetProducer(AControl: TControl): TdxPSCustomContainerItemProducer;
  protected
    procedure BuildNestedControls;
    function CreateHost: TdxReportCell; virtual;
    function CreateItemDefinition(AComponent: TComponent; AnItem: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;
    function CreateNestedControlsIterator: IdxPSContainerIterator; virtual;
    function GetParentHost: TdxReportCell; virtual;
    function HasAvailableChildren(AControl: TControl): Boolean; virtual;
    procedure InitializeHost; virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); virtual;

    function IsAborted: Boolean;
    procedure Progress(const APercentDone: Double);

    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property CurrentControl: TControl read FCurrentControl;
    property Producers[AControl: TControl]: TdxPSCustomContainerItemProducer read GetProducer;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink; AContainer: TWinControl;
      AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil); virtual;
    destructor Destroy; override;

    function Build: TdxReportCell; virtual;
    function BuildControl(AControl: TControl; AParentHost: TdxReportCell = nil): TdxReportVisualItem;
    function BuildNestedContainer(AContainer: TWinControl; AParentHost: TdxReportCell = nil): TdxReportCell;

    class function ContainerClass: TWinControlClass; virtual;
    class function PairClass: TClass; override;
    class procedure Register;
    class procedure Unregister;

    property Container: TWinControl read FContainer;
    property Host: TdxReportCell read FHost;
    property IsRoot: Boolean read GetIsRoot;
    property ParentBuilder: TdxPSContainerBuilder read FParentBuilder;
    property ParentHost: TdxReportCell read GetParentHost;
    property ReportLink: TdxCustomContainerReportLink read FReportLink;
  end;

  { TdxPSContainerReportLinkCustomCache }

  TdxPSContainerReportLinkCustomCache = class(TdxCustomCache)
  private
    FReportLink: TdxCustomContainerReportLink;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink); virtual;
    function ReportLink: TdxCustomContainerReportLink; overload; virtual;
  end;

  { TdxPSCustomProducerCache }

  TdxPSCustomProducerCache = class(TdxPSContainerReportLinkCustomCache)
  private
    function GetItem(Index: Integer): TdxPSCustomProducer;
    function GetProducer(ProducerClass: TdxPSCustomProducerClass; Component: TComponent): TdxPSCustomProducer;
  protected
    property Items[Index: Integer]: TdxPSCustomProducer read GetItem;
  public
    property Producers[ProducerClass: TdxPSCustomProducerClass; Component: TComponent]: TdxPSCustomProducer read GetProducer; default;
  end;

  { TdxCustomContainerReportLinkOptions }

  TdxCustomContainerReportLinkOptionsClass = class of TdxCustomContainerReportLinkOptions;

  TdxCustomContainerReportLinkOptions = class(TPersistent)
  private
    FReportLink: TdxCustomContainerReportLink;
  protected
    procedure Changed; dynamic;
  public
    constructor Create(AReportLink: TdxCustomContainerReportLink); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;

    property ReportLink: TdxCustomContainerReportLink read FReportLink;
  end;

  { TdxCustomContainerReportLinkOptionsBehavior }

  TdxCustomContainerReportLinkOptionsBehaviorClass = class of TdxCustomContainerReportLinkOptionsBehavior;

  TdxCustomContainerReportLinkOptionsBehavior = class(TdxCustomContainerReportLinkOptions)
  private
    FConsumeExistingLinks: Boolean;
    FLabelAutoHeight: Boolean;
    procedure SetConsumeExistingLinks(Value: Boolean);
    procedure SetLabelAutoHeight(AValue: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property ConsumeExistingLinks: Boolean read FConsumeExistingLinks write SetConsumeExistingLinks default True; // obsolete - Aggregated Links always consumed
    property LabelAutoHeight: Boolean read FLabelAutoHeight write SetLabelAutoHeight default True;
  end;

  { TdxCustomContainerReportLinkOptionsDesignerTabs }

  TdxCustomContainerReportLinkOptionsDesignerTabsClass = class of TdxCustomContainerReportLinkOptionsDesignerTabs;

  TdxCustomContainerReportLinkOptionsDesignerTabs = class(TdxCustomContainerReportLinkOptions)
  private
    FAutoHideReportLinksIfEmpty: Boolean;
    FBehaviors: Boolean;
    FControls: Boolean;
    FReportLinks: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property AutoHideReportLinksIfEmpty: Boolean read FAutoHideReportLinksIfEmpty write FAutoHideReportLinksIfEmpty default True;
    property Behaviors: Boolean read FBehaviors write FBehaviors default False;
    property Controls: Boolean read FControls write FControls default True;
    property ReportLinks: Boolean read FReportLinks write FReportLinks default True;
  end;

  TdxCustomContainerReportLinkOptionsItemPlace = class(TdxCustomContainerReportLinkOptions)
  private
    FExpandHeight: Boolean;
    FExpandWidth: Boolean;
    FShrinkHeight: Boolean;
    FShrinkWidth: Boolean;
  protected
    function GetData: Integer; virtual;
    procedure SetData(Value: Integer); virtual;

    procedure ReadData(AStream: TStream); virtual;
    procedure WriteData(AStream: TStream); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;

    function HasHorzResizing: Boolean;
    function HasVertResizing: Boolean;

    procedure SetAll;
    procedure UnsetAll;
  published
    property ExpandHeight: Boolean read FExpandHeight write FExpandHeight default False;
    property ExpandWidth: Boolean read FExpandWidth write FExpandWidth default False;
    property ShrinkHeight: Boolean read FShrinkHeight write FShrinkHeight default False;
    property ShrinkWidth: Boolean read FShrinkWidth write FShrinkWidth default False;
  end;

  TdxCustomContainerReportLinkOptionsPagination = class(TdxCustomContainerReportLinkOptions)
  private
    FControlDetails: Boolean;
    FControls: Boolean;
    procedure SetControlDetails(Value: Boolean);
    procedure SetControls(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property ControlDetails: Boolean read FControlDetails write SetControlDetails default True;
    property Controls: Boolean read FControls write SetControls default True;
  end;

  TdxCustomContainerReportLinkOptionsRefinementsClass = class of TdxCustomContainerReportLinkOptionsRefinements;

  TdxCustomContainerReportLinkOptionsRefinements = class(TdxCustomContainerReportLinkOptions)
  private
    FRootBorders: Boolean;
    procedure SetRootBorders(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property RootBorders: Boolean read FRootBorders write SetRootBorders default False;
  end;

  TdxCustomContainerReportLinkOptionsTransparent = class(TdxCustomContainerReportLinkOptions)
  private
    FContainers: Boolean;
    FControls: Boolean;
    FGraphics: Boolean;
    FRoot: Boolean;
    procedure SetContainters(Value: Boolean);
    procedure SetControls(Value: Boolean);
    procedure SetGraphics(Value: Boolean);
    procedure SetRoot(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property Containers: Boolean read FContainers write SetContainters default True;
    property Controls: Boolean read FControls write SetControls default True;
    property Graphics: Boolean read FGraphics write SetGraphics default False;
    property Root: Boolean read FRoot write SetRoot default True;
  end;

  TdxNodeObject = class
  public
    Caption: string;
    Component: TComponent;
    State: TCheckBoxState;
  end;

  TdxPSReportLinkProcessingStage = (psBefore, psAfter);

  TdxContainerReportLinkCustomDrawItemEvent = procedure(Sender: TdxCustomContainerReportLink;
    ACanvas: TCanvas; AnItem: TAbstractdxReportCellData; AComponent: TComponent; var ADone: Boolean) of object;

  TdxContainerReportLinkGetComponentReportLinkEvent = procedure(Sender: TdxCustomContainerReportLink;
    AComponent: TComponent; var AReportLink: TBasedxReportLink) of object;

  TdxContainerReportLinkGetComponentCaptionEvent = procedure(Sender: TdxCustomContainerReportLink;
    AComponent: TComponent; var ACaption: string) of object;

  TdxContainerReportLinkInitializeItemEvent = procedure(Sender: TdxCustomContainerReportLink;
    AnItem: TdxReportVisualItem; AComponent: TComponent) of object;

  TdxContainerReportLinkInitializeItemOptionsPlaceEvent = procedure(Sender: TdxCustomContainerReportLink;
    AnItem: TdxReportVisualItem; AComponent: TComponent; AOptionsPlace: TdxCustomContainerReportLinkOptionsItemPlace) of object;

  TdxContainerReportLinkInitializeReportLinkEvent = procedure(Sender: TdxCustomContainerReportLink;
    AReportLink: TBasedxReportLink; AStage: TdxPSReportLinkProcessingStage) of object;

  TdxContainerReportLinkIsComponentProcessedEvent = procedure(Sender: TdxCustomContainerReportLink;
    AComponent: TComponent; var AIsProcessed: Boolean) of object;

  { TdxCustomContainerReportLink }

  TdxCustomContainerReportLink = class(TBasedxReportLink, IdxReportLinkController)
  strict private
    FAggregatedReportLinks: TStrings;
    FDefinitions: TList;
    FDelimitersHorz: TList;
    FDelimitersVert: TList;
    FExcludedComponents: TStrings;
    FHiddenComponents: TStrings;
    FOptionsBehavior: TdxCustomContainerReportLinkOptionsBehavior;
    FOptionsDesignerTabs: TdxCustomContainerReportLinkOptionsDesignerTabs;
    FOptionsPagination: TdxCustomContainerReportLinkOptionsPagination;
    FOptionsRefinements: TdxCustomContainerReportLinkOptionsRefinements;
    FOptionsTransparent: TdxCustomContainerReportLinkOptionsTransparent;
    FPreparationFont: TFont;
    FProducerCache: TdxPSCustomProducerCache;
    FScreenCanvas: TdxPSReportRenderCustomCanvas;
    FSupportedCustomDraw: Boolean;

    FOnCustomDrawItem: TdxContainerReportLinkCustomDrawItemEvent;
    FOnGetComponentCaption: TdxContainerReportLinkGetComponentCaptionEvent;
    FOnGetComponentReportLink: TdxContainerReportLinkGetComponentReportLinkEvent;
    FOnInitializeItem: TdxContainerReportLinkInitializeItemEvent;
    FOnInitializeItemOptionsPlace: TdxContainerReportLinkInitializeItemOptionsPlaceEvent;
    FOnInitializeReportLink: TdxContainerReportLinkInitializeReportLinkEvent;
    FOnIsComponentProcessed: TdxContainerReportLinkIsComponentProcessedEvent;

    function GetAggregatedReportLink(Index: Integer): TBasedxReportLink;
    function GetAggregatedReportLinkCount: Integer;
    function GetContainer: TWinControl;
    function GetController: TdxCustomContainerReportLink;
    function GetDefinition(Index: Integer): TdxPSCustomContainerItemDefinition;
    function GetDefinitionByContainerItem(Component: TComponent): TdxPSCustomContainerItemDefinition;
    function GetDefinitionByReportItem(Item: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;
    function GetDefinitionCount: Integer;
    function GetDelimitersHorzCount: Integer;
    function GetDelimitersHorzItem(Index: Integer): Integer;
    function GetDelimitersVertCount: Integer;
    function GetDelimitersVertItem(Index: Integer): Integer;
    function GetDesignWindow: TdxfmCustomContainerDesignWindow;
    function GetExcludedComponent(Index: Integer): TComponent;
    function GetExcludedComponentCount: Integer;
    function GetHiddenComponent(Index: Integer): TComponent;
    function GetHiddenComponentCount: Integer;
    function GetProducerByClass(ProducerClass: TdxPSCustomProducerClass; Component: TComponent): TdxPSCustomProducer;
    function GetRootCell: TdxReportCell;
    function GetTopLevelContainer: TWinControl;
    procedure SetController(Value: TdxCustomContainerReportLink);
    procedure SetOnCustomDrawItem(Value: TdxContainerReportLinkCustomDrawItemEvent);
    procedure SetOptionsBehavior(Value: TdxCustomContainerReportLinkOptionsBehavior);
    procedure SetOptionsDesignerTabs(Value: TdxCustomContainerReportLinkOptionsDesignerTabs);
    procedure SetOptionsPagination(Value: TdxCustomContainerReportLinkOptionsPagination);
    procedure SetOptionsRefinements(Value: TdxCustomContainerReportLinkOptionsRefinements);
    procedure SetOptionsTransparent(Value: TdxCustomContainerReportLinkOptionsTransparent);
    procedure SetSupportedCustomDraw(Value: Boolean);

    procedure LoadAggregatedReportLinks;
    procedure ReadAggregatedReportLinks(Stream: TStream);
    procedure WriteAggregatedReportLinks(Stream: TStream);

    procedure LoadExcludedComponents;
    procedure ReadExcludedComponents(Stream: TStream);
    procedure WriteExcludedComponents(Stream: TStream);

    procedure LoadHiddenComponents;
    procedure ReadHiddenComponents(Stream: TStream);
    procedure WriteHiddenComponents(Stream: TStream);
  protected
    FActiveBuilder: TdxPSContainerBuilder;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AfterDesignReport(ADone: Boolean); override;
    procedure BeforeDesignReport; override;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConvertCoords; override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure DoApplyInDesigner; override;
    procedure DoChangeComponent; override;
    function GetDesignerClass: TdxReportLinkDesignWindowClass; override;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InternalRestoreDefaults; override;

    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    function BuildContainer(AContainer: TWinControl; AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil): TdxReportCell;
    function CreateBuilder(AContainer: TWinControl; AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil): TdxPSContainerBuilder;
    function CreateItemDefinition(AComponent: TComponent; AnItem: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;

    function GetProducer(Component: TComponent): TdxPSCustomProducer;
    // IdxReportLinkController
    function GetControlSiteBounds(AControl: TControl): TRect; virtual;

    procedure DoCustomDrawItem(ACanvas: TCanvas; AnItem: TAbstractdxReportCellData; var ADone: Boolean); dynamic;
    procedure DoGetComponentCaption(AComponent: TComponent; var ACaption: string); dynamic;
    function DoGetReportLink(AComponent: TComponent): TBasedxReportLink; dynamic;
    procedure DoInitializeItem(AnItem: TdxReportVisualItem); dynamic;
    procedure DoInitializeItemOptionsPlace(AnItem: TdxReportVisualItem); dynamic;
    procedure DoInitializeReportLink(AReportLink: TBasedxReportLink; AStage: TdxPSReportLinkProcessingStage); dynamic;
    function DoIsComponentProcessed(AComponent: TComponent): Boolean; dynamic;

    procedure PrepareConstruct; virtual;
    procedure UnprepareConstruct; virtual;

    procedure CreateOptions; virtual;
    procedure DestroyOptions; virtual;
    function GetOptionsBehaviorClass: TdxCustomContainerReportLinkOptionsBehaviorClass; dynamic;
    function GetOptionsDesignerTabsClass: TdxCustomContainerReportLinkOptionsDesignerTabsClass; dynamic;
    function GetOptionsItemPlaceClass: TdxCustomContainerReportLinkOptionsItemPlaceClass; dynamic;
    function GetOptionsPaginationClass: TdxCustomContainerReportLinkOptionsPaginationClass; dynamic;
    function GetOptionsRefinementsClass: TdxCustomContainerReportLinkOptionsRefinementsClass; dynamic;
    function GetOptionsTransparentClass: TdxCustomContainerReportLinkOptionsTransparentClass; dynamic;
    procedure OptionsModified(AnOptions: TdxCustomContainerReportLinkOptions); dynamic;

    procedure AddDefinition(ADefinition: TdxPSCustomContainerItemDefinition);
    procedure ClearDefinitions;
    procedure DeleteDefinition(Index: Integer);
    function FindDefinition(AComponent: TComponent; out AnIndex: Integer): Boolean; overload;
    function FindDefinition(AnItem: TdxReportVisualItem; out AnIndex: Integer): Boolean; overload;
    procedure FreeAndNilDefinitions;

    procedure AddControl(ATreeView: TTreeView; AParent: TTreeNode; AControl: TControl);
    procedure AddHiddenControl(ATreeView: TTreeView; AParent: TTreeNode; AControl: TControl);
    function AddNode(ATreeView: TTreeView; AParent: TTreeNode; AComponent: TComponent; AChecked: Boolean): TTreeNode;
    function CreateNodeObject(AComponent: TComponent; AChecked: Boolean): TdxNodeObject; virtual;
    function IsComponentEditable(AComponent: TComponent): Boolean; virtual;
    procedure LoadControlsTree(ATreeView: TTreeView); virtual;
    procedure LoadHiddenControlsTree(ATreeView: TTreeView); virtual;

    procedure InstallAggregatedReportLinksController(AnInstall: Boolean);

    function GetComponentByName(const AName: string): TComponent;
    function GetPreparedFontIndex(AFont: TFont): Integer; overload;
    function GetPreparedFontIndex(AFont: TFont; AIsFontSubstitutable: Boolean; const AFontName: string;
      AFontColor: TColor; AFontStyle: TFontStyles; AFontOrientation: Integer): Integer; overload;
    function IsComponentProcessed(AComponent: TComponent): Boolean;

    procedure AddDelimiters; virtual;
    procedure CreateRootLookAndFeel; virtual;
    procedure HideDesignerTabs(ADesignWindow: TdxfmCustomContainerDesignWindow); virtual;
    procedure PullReportItems; virtual;
    procedure RepositionControls; virtual;
    //
    function NeedTwoPassRendering: Boolean; override;
    function IsScaleGridLines: Boolean; override;

    property ActiveBuilder: TdxPSContainerBuilder read FActiveBuilder;
    property Controller: TdxCustomContainerReportLink read GetController write SetController;
    property DefinitionCount: Integer read GetDefinitionCount;
    property Definitions[Index: Integer]: TdxPSCustomContainerItemDefinition read GetDefinition;
    property DefinitionsByContainerItem[Component: TComponent]: TdxPSCustomContainerItemDefinition read GetDefinitionByContainerItem;
    property DefinitionsByReportItem[Item: TdxReportVisualItem]: TdxPSCustomContainerItemDefinition read GetDefinitionByReportItem;
    property DelimitersHorz: TList read FDelimitersHorz;
    property DelimitersHorzCount: Integer read GetDelimitersHorzCount;
    property DelimitersHorzItems[Index: Integer]: Integer read GetDelimitersHorzItem;
    property DelimitersVert: TList read FDelimitersVert;
    property DelimitersVertCount: Integer read GetDelimitersVertCount;
    property DelimitersVertItems[Index: Integer]: Integer read GetDelimitersVertItem;
    property PreparationFont: TFont read FPreparationFont;
    property ProducerCache: TdxPSCustomProducerCache read FProducerCache;
    property Producers[Component: TComponent]: TdxPSCustomProducer read GetProducer;
    property RootCell: TdxReportCell read GetRootCell;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read FScreenCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function Aggregable: Boolean; override;

    function CanHideComponent(AComponent: TComponent): Boolean;
    function CanHideComponentByName(const AName: string): Boolean;
    procedure HideComponent(AComponent: TComponent);
    procedure HideComponentByName(const AName: string);
    procedure HideComponents(const AComponents: array of TComponent);
    procedure HideComponentsByName(const ANames: array of string);
    function FindHiddenComponent(AComponent: TComponent; out AnIndex: Integer): Boolean; overload;
    function FindHiddenComponent(AComponent: TComponent): Boolean; overload;
    function FindHiddenComponentByName(const AName: string; out AnIndex: Integer): Boolean; overload;
    function FindHiddenComponentByName(const AName: string): Boolean; overload;
    procedure UnhideAllComponents;
    procedure UnhideComponent(AComponent: TComponent);
    procedure UnhideComponentByName(const AName: string);
    procedure UnhideComponents(const AComponents: array of TComponent);
    procedure UnhideComponentsByName(const ANames: array of string);

    procedure HideStandardControls;
    procedure UnhideStandardControls;

    function CanExcludeComponent(AComponent: TComponent): Boolean;
    function CanExcludeComponentByName(const AName: string): Boolean;
    procedure ExcludeComponent(AComponent: TComponent);
    procedure ExcludeComponentByName(const AName: string);
    procedure ExcludeComponents(const AComponents: array of TComponent);
    procedure ExcludeComponentsByName(const ANames: array of string);
    function FindExcludedComponent(AComponent: TComponent; out AnIndex: Integer): Boolean; overload;
    function FindExcludedComponent(AComponent: TComponent): Boolean; overload;
    function FindExcludedComponentByName(const AName: string; out AnIndex: Integer): Boolean; overload;
    function FindExcludedComponentByName(const AName: string): Boolean; overload;
    procedure UnexcludeAllComponents;
    procedure UnexcludeComponent(AComponent: TComponent);
    procedure UnexcludeComponentByName(const AName: string);
    procedure UnexcludeComponents(const AComponents: array of TComponent);
    procedure UnexcludeComponentsByName(const ANames: array of string);

    procedure AggregateLink(AReportLink: TBasedxReportLink);
    procedure DisaggregateAllLinks;
    procedure DisaggregateInconsistentLinks;
    procedure DisaggregateLink(AReportLink: TBasedxReportLink);
    function FindAggregatedLinkByComponent(AComponent: TComponent): TBasedxReportLink;
    function HasInconsistentlyAggregatedLinks: Boolean;
    function IsLinkAggregable(AReportLink: TBasedxReportLink): Boolean;
    function IsLinkAggregated(AReportLink: TBasedxReportLink): Boolean;
    function IsLinkAggregatedConsistently(AReportLink: TBasedxReportLink): Boolean;

    property AggregatedReportLinkCount: Integer read GetAggregatedReportLinkCount;
    property AggregatedReportLinks[Index: Integer]: TBasedxReportLink read GetAggregatedReportLink;
    property Container: TWinControl read GetContainer;
    property DesignWindow: TdxfmCustomContainerDesignWindow read GetDesignWindow;
    property ExcludedComponentCount: Integer read GetExcludedComponentCount;
    property ExcludedComponents[Index: Integer]: TComponent read GetExcludedComponent;
    property HiddenComponentCount: Integer read GetHiddenComponentCount;
    property HiddenComponents[Index: Integer]: TComponent read GetHiddenComponent;
    property ProducersByClass[ProducerClass: TdxPSCustomProducerClass; Component: TComponent]: TdxPSCustomProducer read GetProducerByClass;
    property TopLevelContainer: TWinControl read GetTopLevelContainer;
  published
    property Color;
    property Font;
    property OptionsBehavior: TdxCustomContainerReportLinkOptionsBehavior read FOptionsBehavior write SetOptionsBehavior;
    property OptionsDesignerTabs: TdxCustomContainerReportLinkOptionsDesignerTabs read FOptionsDesignerTabs write SetOptionsDesignerTabs;
    //property OptionsItemPlace: TdxCustomContainerReportLinkOptionsItemPlace read FOptionsItemPlace write SetOptionsItemPlace; {.2}
    property OptionsPagination: TdxCustomContainerReportLinkOptionsPagination read FOptionsPagination write SetOptionsPagination;
    property OptionsRefinements: TdxCustomContainerReportLinkOptionsRefinements read FOptionsRefinements write SetOptionsRefinements;
    property OptionsTransparent: TdxCustomContainerReportLinkOptionsTransparent read FOptionsTransparent write SetOptionsTransparent;
    property ScaleFonts;
    property SupportedCustomDraw: Boolean read FSupportedCustomDraw write SetSupportedCustomDraw default False;

    property OnCustomDrawItem: TdxContainerReportLinkCustomDrawItemEvent read FOnCustomDrawItem write SetOnCustomDrawItem;
    property OnGetComponentCaption: TdxContainerReportLinkGetComponentCaptionEvent read FOnGetComponentCaption write FOnGetComponentCaption;
    property OnGetComponentReportLink: TdxContainerReportLinkGetComponentReportLinkEvent read FOnGetComponentReportLink write FOnGetComponentReportLink;
    property OnInitializeItem: TdxContainerReportLinkInitializeItemEvent read FOnInitializeItem write FOnInitializeItem;
    property OnInitializeItemOptionsPlace: TdxContainerReportLinkInitializeItemOptionsPlaceEvent read FOnInitializeItemOptionsPlace write FOnInitializeItemOptionsPlace;
    property OnInitializeReportLink: TdxContainerReportLinkInitializeReportLinkEvent read FOnInitializeReportLink write FOnInitializeReportLink;
    property OnIsComponentProcessed: TdxContainerReportLinkIsComponentProcessedEvent read FOnIsComponentProcessed write FOnIsComponentProcessed;
  end;

  TdxfmCustomContainerDesignWindow = class(TStandarddxReportLinkDesignWindow)
    btnControlsCheckAll: TcxButton;
    btnControlsExpandAll: TcxButton;
    btnHiddenControlsCheckAll: TcxButton;
    btnHiddenControlsExpandAll: TcxButton;
    btnLinksDesign: TcxButton;
    btnLinksRemoveInconsistents: TcxButton;
    chbxAutoWidth: TcxCheckBox;
    chbxExpandedGroups: TcxCheckBox;
    chbxPaginateByControlDetails: TcxCheckBox;
    chbxPaginateByControls: TcxCheckBox;
    chbxPaginateByGroups: TcxCheckBox;
    chbxPaginateByItems: TcxCheckBox;
    chbxRiseActiveTabOntoTop: TcxCheckBox;
    chbxSkipEmptyGroups: TcxCheckBox;
    chbxTransparentContainers: TcxCheckBox;
    chbxTransparentControls: TcxCheckBox;
    chbxTransparentGraphics: TcxCheckBox;
    chbxTransparentGroups: TcxCheckBox;
    chbxTransparentItems: TcxCheckBox;
    chbxTransparentRoot: TcxCheckBox;
    chbxUnwrapTabs: TcxCheckBox;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem32: TdxLayoutItem;
    dxLayoutItem33: TdxLayoutItem;
    dxLayoutItem34: TdxLayoutItem;
    dxLayoutItem35: TdxLayoutItem;
    dxLayoutItem36: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    ilControls: TcxImageList;
    ilControlsPopup: TcxImageList;
    imgExpanding: TcxImage;
    imgGroups: TcxImage;
    imgPagination: TcxImage;
    imgSize: TcxImage;
    imgTabs: TcxImage;
    lbGroups: TcxLabel;
    lblAggregatedLinks: TdxLayoutItem;
    lblPagination: TcxLabel;
    lblSize: TcxLabel;
    lblTransparents: TcxLabel;
    lbTabs: TcxLabel;
    lbxAggregatedLinks: TcxListBox;
    lbxAvailableLinks: TcxListBox;
    libtnLinksRemoveInconsistents: TdxLayoutItem;
    lichbxAutoWidth: TdxLayoutItem;
    lichbxPaginateByGroups: TdxLayoutItem;
    lichbxPaginateByItems: TdxLayoutItem;
    lichbxTransparentGroups: TdxLayoutItem;
    lichbxTransparentItems: TdxLayoutItem;
    liimgSize: TdxLayoutItem;
    lilblSize: TdxLayoutItem;
    miControlsCheckAll: TMenuItem;
    miControlsCheckAllChildren: TMenuItem;
    miControlsCheckStandardControls: TMenuItem;
    miControlsExpandAll: TMenuItem;
    miControlsUncheckAllChildren: TMenuItem;
    miControlsUncheckStandardControls: TMenuItem;
    miLine1: TMenuItem;
    miLine2: TMenuItem;
    miLine3: TMenuItem;
    PageControl: TdxLayoutGroup;
    pmControls: TPopupMenu;
    pnlAvailableLinks: TdxLayoutItem;
    pnlMoveButtonsSite: TdxLayoutGroup;
    sbtnAdd: TcxButton;
    sbtnRemove: TcxButton;
    tshBehaviors: TdxLayoutGroup;
    tshControls: TdxLayoutGroup;
    tshHiddenControls: TdxLayoutGroup;
    tshOptions: TdxLayoutGroup;
    tshReportLinks: TdxLayoutGroup;
    tvControls: TcxTreeView;
    tvHiddenControls: TcxTreeView;

    procedure btnLinksRemoveInconsistentsClick(Sender: TObject);
    procedure DesignClick(Sender: TObject);
    procedure GroupsClick(Sender: TObject);
    procedure HideStandardControlsClick(Sender: TObject);
    procedure lbxAggregatedLinksClick(Sender: TObject);
    procedure lbxAggregatedLinksDblClick(Sender: TObject);
    procedure lbxAggregatedLinksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxAggregatedLinksDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbxAggregatedLinksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbxAvailableLinksDblClick(Sender: TObject);
    procedure lbxAvailableLinksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxAvailableLinksDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbxAvailableLinksDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure lbxAvailableLinksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageControl1Change(Sender: TObject);
    procedure PaginationClick(Sender: TObject);
    procedure pmControlsPopup(Sender: TObject);
    procedure sbtnAddClick(Sender: TObject);
    procedure sbtnRemoveClick(Sender: TObject);
    procedure SizeClick(Sender: TObject);
    procedure TabsClick(Sender: TObject);
    procedure TransparentClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCheckAllChildrenClick(Sender: TObject);
    procedure TreeViewCheckAllClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeViewExpandAllClick(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewUncheckAllChildrenClick(Sender: TObject);
    procedure UnhideStandardControlsClick(Sender: TObject);
  private
    FAreHiddenControlsChanged: Boolean;
    FLastActiveTab: Integer;

    function GetAggregatedLink(Index: Integer): TBasedxReportLink;
    function GetAggregatedLinkCount: Integer;
    function GetAggregatedLinkSelected(Index: Integer): Boolean;
    function GetAvailableLink(Index: Integer): TBasedxReportLink;
    function GetAvailableLinkCount: Integer;
    function GetAvailableLinkSelected(Index: Integer): Boolean;
    function GetReportLink: TdxCustomContainerReportLink;
    function GetSelectedReportLink: TBasedxReportLink;

    procedure DoCheckAllChildren(ANode: TTreeNode; AChecked: Boolean);
    procedure DoToggleNodeState(ANode: TTreeNode);

    function CanAggregate: Boolean;
    function CanDesign: Boolean;
    function CanDisaggregate: Boolean;
    function CanRemoveInconsistents: Boolean;
    //function CanSelectAll(AListBox: TListBox): Boolean;
    procedure DoAggregateSelectedLinks;
    procedure DoDisaggregateSelectedLinks;
    procedure DoSelectAll(AListBox: TcxListBox);
    procedure RefreshAggregatedLinks;
    procedure RefreshAvailableLinks;

  protected
    procedure BeforeConstruction; override;
    procedure DoInitialize; override;
    procedure LoadStrings; override;
    procedure UpdateControlsState; override;

    procedure LoadGroupsIcons; override;
    function GetActiveTreeView: TcxTreeView; virtual;
    procedure InitializeControlsTree; virtual;
    procedure InitializeHiddenControlsTree; virtual;
    function IsBoldNode(ANode: TTreeNode): Boolean; virtual;
    procedure RefreshControlsTree;
    procedure RefreshHiddenControlsTree;
    procedure RefreshReportLinksList;

    procedure SetOptionsGroupsByIndex(AnIndex: Integer; AValue: Boolean); virtual;
    procedure SetOptionsPaginationByIndex(AnIndex: Integer; AValue: Boolean); virtual;
    //procedure SetOptionsPlaceByIndex(AnIndex: Integer; AValue: Boolean); virtual; {.2}
    procedure SetOptionsSizeByIndex(AnIndex: Integer; AValue: Boolean); virtual;
    procedure SetOptionsTabsByIndex(AnIndex: Integer; AValue: Boolean); virtual;
    procedure SetOptionsTransparentByIndex(AnIndex: Integer; AValue: Boolean); virtual;

    procedure TreeView_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TreeView_BeginUpdate;
    procedure TreeView_CheckAllChildren(ANode: TTreeNode; AChecked: Boolean);
    procedure TreeView_EndUpdate;
    procedure TreeView_FreeNodeObjects(ATreeView: TTreeView);
    function TreeView_HasCheckedChildren(ANode: TTreeNode): Boolean;
    function TreeView_HasRoot(ATreeView: TTreeView): Boolean;
    function TreeView_HasUncheckedChildren(ANode: TTreeNode): Boolean;
    function TreeView_GetNodeObject(ANode: TTreeNode): TdxNodeObject;
    function TreeView_getRoot(ATreeView: TTreeView): TTreeNode; // API function TreeView_GetRoot already exists
    function TreeView_IsNodeEditable(ANode: TTreeNode): Boolean;
    procedure TreeView_NormalizeNode(ANode: TTreeNode);
    procedure TreeView_SetNodeState(ANode: TTreeNode; AState: TCheckBoxState);
    procedure TreeView_ToggleNodeState(ANode: TTreeNode);
    procedure TreeView_UpdateNodeImage(ANode: TTreeNode);
    procedure TreeView_UpdateNodesState(ANode: TTreeNode);

    property ActiveTreeView: TcxTreeView read GetActiveTreeView;
    property SelectedReportLink: TBasedxReportLink read GetSelectedReportLink;

    property AggregatedLinkCount: Integer read GetAggregatedLinkCount;
    property AggregatedLinks[Index: Integer]: TBasedxReportLink read GetAggregatedLink;
    property AggregatedLinkSelected[Index: Integer]: Boolean read GetAggregatedLinkSelected;
    property AvailableLinkCount: Integer read GetAvailableLinkCount;
    property AvailableLinks[Index: Integer]: TBasedxReportLink read GetAvailableLink;
    property AvailableLinkSelected[Index: Integer]: Boolean read GetAvailableLinkSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ReportLink: TdxCustomContainerReportLink read GetReportLink;
  end;

function dxPSIsComponentContainer(AComponentClass: TClass): Boolean; overload;
function dxPSIsComponentContainer(AComponent: TObject{TComponent}): Boolean; overload;
procedure dxPSRegisterContainer(AContainerClass: TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil{TdxfmCustomContainerDesignWindow});
procedure dxPSRegisterContainers(const AContainerClasses: array of TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil{TdxfmCustomContainerDesignWindow});
procedure dxPSUnregisterContainer(AContainerClass: TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil{TdxfmCustomContainerDesignWindow});
procedure dxPSUnregisterContainers(const AContainerClasses: array of TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil{TdxfmCustomContainerDesignWindow});

function dxPSMakeFriendlyNameFromStrings(AStrings: TStrings): string;

const
  BevelShapeMap: array[TBevelShape] of TdxCellSides =
    (csAll, csAll, [csTop], [csBottom], [csLeft], [csRight], []);
  BevelStyleMap: array[TBevelStyle, TBevelShape] of TdxPSCellBorderClass =
    ((TdxPSCellSunkenSoftBorder, TdxPSCellEtchedBorder, TdxPSCellRaisedSoftBorder,
      TdxPSCellRaisedSoftBorder, TdxPSCellRaisedSoftBorder, TdxPSCellRaisedSoftBorder, TdxPSCellNullBorder),
     (TdxPSCellRaisedSoftBorder, TdxPSCellBumpedBorder, TdxPSCellRaisedBorder,
      TdxPSCellRaisedBorder, TdxPSCellRaisedBorder, TdxPSCellRaisedBorder, TdxPSCellNullBorder));
  BorderStyleMap: array[TBorderStyle] of TdxCellSides = ([], csAll);
  BrushStyleMap: array[TBrushStyle] of TdxPSFillPatternClass =
    (TdxPSSolidFillPattern, TdxPSSolidFillPattern, TdxPSHorizontalFillPattern, TdxPSVerticalFillPattern,
     TdxPSFDiagonalFillPattern, TdxPSBDiagonalFillPattern, TdxPSCrossFillPattern,
     TdxPSDiagCrossFillPattern);
  CheckAlignmentMap: array[TLeftRight] of TdxCellCheckPos = (ccpRight, ccpLeft);
  Ctl3DBorderClassMap: array[Boolean] of TdxPSCellBorderClass = (TdxPSCellUltraFlatBorder, TdxPSCellSunkenBorder);
  PanelBevelsMap: array[TPanelBevel, TPanelBevel] of TdxPSCellBorderClass =
    ((TdxPSCellNullBorder, TdxPSCellSunkenSoftBorder, TdxPSCellRaisedSoftBorder, TdxPSCellRaisedSoftBorder),
     (TdxPSCellSunkenSoftBorder, TdxPSCellSunkenBorder, TdxPSCellBumpedBorder, TdxPSCellBumpedBorder),
     (TdxPSCellRaisedSoftBorder, TdxPSCellEtchedBorder, TdxPSCellRaisedBorder, TdxPSCellRaisedBorder),
     (TdxPSCellRaisedSoftBorder, TdxPSCellEtchedBorder, TdxPSCellRaisedBorder, TdxPSCellRaisedBorder));
  PanelSingleBorderMap: array[Boolean] of TdxPSCellBorderClass = (TdxPSCellRaisedBorder, TdxPSCellEtchedBorder);
  ShapeTypeMap: array[TShapeType] of TdxReportCellShapeClass =
    (TdxReportCellRectangle, TdxReportCellSquare, TdxReportCellRoundRect,
     TdxReportCellRoundSquare, TdxReportCellEllipse, TdxReportCellCircle);

  MaxCaptionLength: Integer = 64;

  ExpandHeightBit = $00000001;
  ExpandHeightOffset = $00000000;
  ExpandWidthBit = $00000002;
  ExpandWidthOffset = $00000001;
  ShrinkHeightBit = $00000004;
  ShrinkHeightOffset = $00000002;
  ShrinkWidthBit = $00000008;
  ShrinkWidthOffset = $00000003;

  StandardHiddenControlCount = 6;
  StandardHiddenControls: array[0..StandardHiddenControlCount - 1] of string =
    ('TScrollBar', 'TSplitter', 'TControlBar', 'TToolBar', 'TCoolBar', 'TPageScroller');

implementation

{$R *.dfm}

uses
  SysUtils, Math, dxCore, dxPSGlbl, dxPSUtl, dxPSRes, dxPSPopupMan, dxPSImgs;

const
  StandardContainerCount = 13;
  StandardContainers: array[0..StandardContainerCount - 1] of TWinControlClass =
    (TPanel, TScrollBox, TTabControl, TTabSheet, TPageControl, TTabPage,
     TTabbedNotebook, TPage, TNotebook, TCustomForm, TCustomFrame, TcxGroupBox, TcxScrollBox);

  DefaultControlDisabledContentColor: TColor = clBtnFace;
  DefaultControlDisabledTextColor: TColor = clGrayText;

//{.1} - RootDesigner
//{.2} - ItemOptionsPlace

type
  TBasedxReportLinkAccess = class(TBasedxReportLink);


  TCustomCheckBoxAccess = class(TCustomCheckBox);
  TCustomComboBoxAccess = class(TCustomComboBox);
  TCustomComboBoxExAccess = class(TCustomComboBoxEx);
  TCustomEditAccess = class(TCustomEdit);
  TCustomGroupBoxAccess = class(TCustomGroupBox);
  TCustomHotKeyAccess = class(TCustomHotKey);
  TCustomLabelAccess = class(TCustomLabel);
  TCustomMemoAccess = class(TCustomMemo);
  TCustomPanelAccess = class(TCustomPanel);
  TCustomRadioGroupAccess = class(TCustomRadioGroup);
  TCustomStaticTextAccess = class(TCustomStaticText);

  TdxPSNativePrintableControlProducerFactory = class(TdxCustomClassMaps)
  private
    function GetProducerClass(Component: TComponent): TdxPSNativePrintableControlProducerClass;
  public
    class function Instance: TdxPSNativePrintableControlProducerFactory; reintroduce; overload;
    class procedure ReleaseInstance; override;
    property ProducerClasses[Component: TComponent]: TdxPSNativePrintableControlProducerClass read GetProducerClass; default;
  end;

  TdxPSContainerItemProducerFactory = class(TdxCustomClassMaps)
  private
    function GetProducerClass(Component: TComponent; IsRoot: Boolean): TdxPSCustomProducerClass;
  public
    class function Instance: TdxPSContainerItemProducerFactory; reintroduce; overload;
    class procedure ReleaseInstance; override;
    property ProducerClasses[Component: TComponent; IsRoot: Boolean]: TdxPSCustomProducerClass read GetProducerClass; default;
  end;

  TdxPSProducerHelperFactory = class(TdxCustomClassMaps)
  private
    function GetItem(Index: Integer): TdxPSCustomProducerClass;
    function GetProducerClass(ProducerClass: TdxPSCustomProducerClass): TdxPSCustomProducerClass;
  public
    class function Instance: TdxPSProducerHelperFactory; reintroduce; overload;
    property Items[Index: Integer]: TdxPSCustomProducerClass read GetItem;
    property ProducerClasses[ProducerClass: TdxPSCustomProducerClass]: TdxPSCustomProducerClass read GetProducerClass; default;
  end;

  TdxPSContainerBuilderFactory = class(TdxCustomClassMaps)
  private
    function GetBuilderClass(Control: TWinControl): TdxPSContainerBuilderClass;
  protected
    function CreateBuilder(AReportLink: TdxCustomContainerReportLink; AContainer: TWinControl;
      AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil): TdxPSContainerBuilder;
  public
    class function Instance: TdxPSContainerBuilderFactory; reintroduce; overload;
    class procedure ReleaseInstance; override;
    property BuilderClasses[Control: TWinControl]: TdxPSContainerBuilderClass read GetBuilderClass; default;
  end;

var
  FInternalComponentPrinter: TdxComponentPrinter;
  FIsNativePrintableControlProducerFactoryReleased: Boolean = False;
  FIsContainerItemProducerFactoryReleased: Boolean = False;
  FIsContainerBuilderFactoryReleased: Boolean = False;

function InternalComponentPrinter: TdxComponentPrinter;
begin
  if FInternalComponentPrinter = nil then
  begin
    FInternalComponentPrinter := TdxComponentPrinter.Create(nil);
    FInternalComponentPrinter.Name := 'ContainerReportLinkComponentPrinter';
  end;
  Result := FInternalComponentPrinter;
end;

{ utility routines }

function dxPSMakeFriendlyNameFromStrings(AStrings: TStrings): string;
const
  CR = #13;
  LF = #10;
var
  P: Integer;
begin
  Result := AStrings[0];
  if Length(Result) > MaxCaptionLength then
  begin
    Delete(Result, MaxCaptionLength, Length(Result) - MaxCaptionLength);
    Result := Result + '...';
  end;

  repeat
    P := Pos(LF, Result);
    if P <> 0 then
      Delete(Result, P, 1);

    P := Pos(CR, Result);
    if P <> 0 then
      Delete(Result, P, 1);

    if P <> 0 then
      Insert(' ', Result, P);
  until P = 0;
end;

{ Registration routines }

function dxPSIsComponentContainer(AComponentClass: TClass): Boolean;
begin
  Result := TdxCustomContainerReportLink.Supports(AComponentClass);
end;

function dxPSIsComponentContainer(AComponent: TObject{TComponent}): Boolean;
begin
  Result := TdxCustomContainerReportLink.Supports(AComponent);
end;

procedure dxPSRegisterContainer(AContainerClass: TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil);
begin
  if ADesignerClass = nil then
    ADesignerClass := TdxfmCustomContainerDesignWindow;
  dxPSRegisterReportLink(TdxCustomContainerReportLink, AContainerClass, ADesignerClass);
end;

procedure dxPSRegisterContainers(const AContainerClasses: array of TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil);
var
  I: Integer;
begin
  if ADesignerClass = nil then
    ADesignerClass := TdxfmCustomContainerDesignWindow;
  for I := 0 to High(AContainerClasses) do
    dxPSRegisterContainer(AContainerClasses[I], ADesignerClass);
end;

procedure dxPSUnregisterContainer(AContainerClass: TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil);
begin
  if ADesignerClass = nil then
    ADesignerClass := TdxfmCustomContainerDesignWindow;
  dxPSUnregisterReportLink(TdxCustomContainerReportLink, AContainerClass, ADesignerClass);
end;

procedure dxPSUnregisterContainers(const AContainerClasses: array of TWinControlClass;
  ADesignerClass: TdxReportLinkDesignWindowClass = nil);
var
  I: Integer;
begin
  if ADesignerClass = nil then
    ADesignerClass := TdxfmCustomContainerDesignWindow;
  for I := 0 to High(AContainerClasses) do
    dxPSUnregisterContainer(AContainerClasses[I], ADesignerClass);
end;

{ Helpers }

{ CheckBox Helpers }

function CheckBox_GetAlignment(AControl: TCustomCheckBox): TAlignment;
begin
  Result := TCustomCheckBoxAccess(AControl).Alignment;
end;

function CheckBox_GetState(AControl: TCustomCheckBox): TCheckBoxState;
begin
  Result := TCustomCheckBoxAccess(AControl).State;
end;

{ ComboBox Helpers }

function ComboBox_GetStyle(AControl: TCustomComboBox): TComboBoxStyle;
begin
  Result := TCustomComboBoxAccess(AControl).Style;
end;

function ComboBox_GetCurrentText(AControl: TCustomComboBox): string;
begin
  if ComboBox_GetStyle(AControl) = csDropDown then
    Result := Control_GetText(AControl)
  else
    if AControl.ItemIndex <> -1 then
      Result := AControl.Items[AControl.ItemIndex]
    else
      Result := ''
end;

{ Edit Helpers }

function Edit_GetBorderStyle(AControl: TCustomEdit): TBorderStyle;
begin
  Result := TCustomEditAccess(AControl).BorderStyle;
end;

{ HotKey Helpers }

function HotKey_GetHotKey(AControl: TCustomHotKey): TShortCut;
begin
  Result := TCustomHotKeyAccess(AControl).HotKey;
end;

{ Label Helpers }

function Label_GetAlignment(AControl: TCustomLabel): TAlignment;
begin
  Result := TCustomLabelAccess(AControl).Alignment;
end;

function Label_GetAutoSize(AControl: TCustomLabel): Boolean;
begin
  Result := TCustomLabelAccess(AControl).AutoSize;
end;

function Label_GetText(AControl: TCustomLabel): string;
begin
  Result := TCustomLabelAccess(AControl).GetLabelText;
end;

function Label_GetLayout(AControl: TCustomLabel): TTextLayout;
begin
  Result := TCustomLabelAccess(AControl).Layout;
end;

function Label_GetWordWrap(AControl: TCustomLabel): Boolean;
begin
  Result := TCustomLabelAccess(AControl).WordWrap;
end;

{ Memo Helpers }

function Memo_GetAlignment(AControl: TCustomMemo): TAlignment;
begin
  Result := TCustomMemoAccess(AControl).Alignment;
end;

function Memo_GetLines(AControl: TCustomMemo): TStrings;
begin
  Result := TCustomMemoAccess(AControl).Lines;
end;

{ Panel Helpers }

function Panel_GetAlignment(AControl: TCustomPanel): TAlignment;
begin
  Result := TCustomPanelAccess(AControl).Alignment;
end;

function Panel_GetBorderStyle(AControl: TCustomPanel): TBorderStyle;
begin
  Result := TCustomPanelAccess(AControl).BorderStyle;
end;

function Panel_GetBevelInner(AControl: TCustomPanel): TPanelBevel;
begin
  Result := TCustomPanelAccess(AControl).BevelInner;
end;

function Panel_GetBevelOuter(AControl: TCustomPanel): TPanelBevel;
begin
  Result := TCustomPanelAccess(AControl).BevelOuter;
end;

{ RadioGroup Helpers }

function RadioGroup_GetColumns(AControl: TCustomRadioGroup): Integer;
begin
  Result := TCustomRadioGroupAccess(AControl).Columns;
end;

function RadioGroup_GetItemIndex(AControl: TCustomRadioGroup): Integer;
begin
  Result := TCustomRadioGroupAccess(AControl).ItemIndex;
end;

function RadioGroup_GetItems(AControl: TCustomRadioGroup): TStrings;
begin
  Result := TCustomRadioGroupAccess(AControl).Items;
end;

{ StaticText Helpers }

function StaticText_GetAlignment(AControl: TCustomStaticText): TAlignment;
begin
  Result := TCustomStaticTextAccess(AControl).Alignment;
end;

function StaticText_GetBorderStyle(AControl: TCustomStaticText): TStaticBorderStyle;
begin
  Result := TCustomStaticTextAccess(AControl).BorderStyle
end;

{ TdxPSNativePrintableControlProducerFactory }

function dxPSNativePrintableControlProducerFactory: TdxPSNativePrintableControlProducerFactory;
begin
  Result := TdxPSNativePrintableControlProducerFactory.Instance;
end;

class function TdxPSNativePrintableControlProducerFactory.Instance: TdxPSNativePrintableControlProducerFactory;
begin
  Result := inherited Instance as TdxPSNativePrintableControlProducerFactory;
end;

class procedure TdxPSNativePrintableControlProducerFactory.ReleaseInstance;
begin
  inherited;
  FIsNativePrintableControlProducerFactoryReleased := True;
end;

function TdxPSNativePrintableControlProducerFactory.GetProducerClass(
  Component: TComponent): TdxPSNativePrintableControlProducerClass;
begin
  Result := TdxPSNativePrintableControlProducerClass(PairClasses[Component.ClassType]);
end;

{ TdxProducersFactory }

function dxPSContainerItemProducerFactory: TdxPSContainerItemProducerFactory;
begin
  Result := TdxPSContainerItemProducerFactory.Instance;
end;

class function TdxPSContainerItemProducerFactory.Instance: TdxPSContainerItemProducerFactory;
begin
  Result := inherited Instance as TdxPSContainerItemProducerFactory;
end;

class procedure TdxPSContainerItemProducerFactory.ReleaseInstance;
begin
  inherited ReleaseInstance;
  FIsContainerBuilderFactoryReleased := True;
end;

function TdxPSContainerItemProducerFactory.GetProducerClass(Component: TComponent;
  IsRoot: Boolean): TdxPSCustomProducerClass;
begin
  if not IsRoot then
    Result := dxPSNativePrintableControlProducerFactory.ProducerClasses[Component]
  else
    Result := nil;

  if (Result = nil) or not TdxPSNativePrintableControlProducerClass(Result).HasReportLink(Component) then
  begin
    Result := TdxPSCustomProducerClass(PairClasses[Component.ClassType]);
    if Result.HelperProducer <> nil then
      Result := Result.HelperProducer;
  end;
end;

{ TdxPSProducerHelperFactory }

function dxPSProducerHelperFactory: TdxPSProducerHelperFactory;
begin
  Result := TdxPSProducerHelperFactory.Instance;
end;

class function TdxPSProducerHelperFactory.Instance: TdxPSProducerHelperFactory;
begin
  Result := inherited Instance as TdxPSProducerHelperFactory;
end;

function TdxPSProducerHelperFactory.GetItem(Index: Integer): TdxPSCustomProducerClass;
begin
  Result := TdxPSCustomProducerClass(inherited Items[Index]);
end;

function TdxPSProducerHelperFactory.GetProducerClass(ProducerClass: TdxPSCustomProducerClass): TdxPSCustomProducerClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if ProducerClass = Result.BuddyClass then Exit;
  end;
  Result := nil;
end;

{ TdxPSContainerBuilderFactory }

function dxContainerBuilderFactory: TdxPSContainerBuilderFactory;
begin
  Result := TdxPSContainerBuilderFactory.Instance;
end;

class function TdxPSContainerBuilderFactory.Instance: TdxPSContainerBuilderFactory;
begin
  Result := inherited Instance as TdxPSContainerBuilderFactory;
end;

class procedure TdxPSContainerBuilderFactory.ReleaseInstance;
begin
  inherited;
  FIsContainerBuilderFactoryReleased := True;
end;

function TdxPSContainerBuilderFactory.CreateBuilder(AReportLink: TdxCustomContainerReportLink;
  AContainer: TWinControl; AParentBuilder: TdxPSContainerBuilder;
  AParentHost: TdxReportCell = nil): TdxPSContainerBuilder;
begin
  Result := BuilderClasses[AContainer].Create(AReportLink, AContainer, AParentBuilder, AParentHost);
end;

function TdxPSContainerBuilderFactory.GetBuilderClass(Control: TWinControl): TdxPSContainerBuilderClass;
begin
  Result := TdxPSContainerBuilderClass(PairClasses[Control.ClassType]);
end;

{ TdxReportWinControlHost }

function TdxReportWinControlHost.MeasureHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;

  procedure MeasureItem(AnItem: TdxReportVisualItem; var AValue: Integer);
  var
    V: Integer;
  begin
    if AnItem.Visible then
    begin
      V := AnItem.Top + AnItem.Height;
      if AValue < V then AValue := V;
    end;
  end;

  function MeasureNestedCells: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to CellCount - 1 do
      MeasureItem(Cells[I], Result);
  end;

  function MeasureNestedDataItems: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to DataItemCount - 1 do
      MeasureItem(DataItems[I], Result);
  end;

begin
  Result := Max(MeasureNestedDataItems, MeasureNestedCells);
  if Result <> 0 then Inc(Result, 2);
end;

function TdxReportWinControlHost.MeasureWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;

  procedure MeasureItem(AnItem: TdxReportVisualItem; var AValue: Integer);
  var
    V: Integer;
  begin
    if AnItem.Visible then
    begin
      V := AnItem.Left + AnItem.Width;
      if AValue < V then AValue := V;
    end;
  end;

  function MeasureNestedCells: Integer;
  var
    I: Integer;
    Item: TdxReportVisualItem;
  begin
    Result := 0;
    for I := 0 to CellCount - 1 do
    begin
      Item := Cells[I];
      if Item <> ControlItem then MeasureItem(Item, Result);
    end;
  end;

  function MeasureNestedDataItems: Integer;
  var
    I: Integer;
    Item: TdxReportVisualItem;
  begin
    Result := 0;
    for I := 0 to DataItemCount - 1 do
    begin
      Item := DataItems[I];
      {if Item <> ControlItem then }MeasureItem(Item, Result);
    end;
  end;

begin
  Result := Max(MeasureNestedDataItems, MeasureNestedCells);
  if Result <> 0 then Inc(Result, 2);
end;

function TdxReportWinControlHost.GetControlItem: TdxReportVisualItem;
begin
  if (DataItemCount <> 0) and (DataItems[0].Data <> 0) then
    Result := DataItems[0]
  else
    if (CellCount <> 0) and (Cells[0].Data <> 0) then
      Result := Cells[0]
    else
      Result := nil;
end;

procedure TdxReportWinControlHost.BoundsChanged;
begin
  if HasControlItem then
  begin
    ControlItem.Width := Width;
    ControlItem.Height := Height;
  end;
end;

function TdxReportWinControlHost.GetHasControlItem: Boolean;
begin
  Result := ControlItem <> nil;
end;

{ TdxReportNativePrintableControlHost }

function TdxReportNativePrintableControlHost.GetControlItem: TdxReportVisualItem;
begin
  if CellCount <> 0 then
    Result := Cells[0]
  else
    Result := nil;
end;

procedure TdxReportNativePrintableControlHost.BoundsChanged;
begin
  inherited;
end;

{ TdxPSCustomContainerItemDefinition }

constructor TdxPSCustomContainerItemDefinition.Create(AReportLink: TdxCustomContainerReportLink;
  AComponent: TComponent; AReportItem: TdxReportVisualItem);
begin
  inherited Create;
  FReportLink := AReportLink;
  FComponent := AComponent;
  FReportItem := AReportItem;
  FOptionsPlace := AReportLink.GetOptionsItemPlaceClass.Create(AReportLink);
//  FOptionsPlace.Assign(AReportLink.OptionsItemPlace); {.2}
end;

destructor TdxPSCustomContainerItemDefinition.Destroy;
begin
  FreeAndNil(FOptionsPlace);
  inherited;
end;

function TdxPSCustomContainerItemDefinition.OptionsPagination: TdxCustomContainerReportLinkOptionsPagination;
begin
  Result := ReportLink.OptionsPagination;
end;

procedure TdxPSCustomContainerItemDefinition.AddDelimitersHorz(AList: TList);
begin
end;

procedure TdxPSCustomContainerItemDefinition.AddDelimitersVert(AList: TList);
begin
end;

procedure TdxPSCustomContainerItemDefinition.AddReportItemToDelimitersHorz(AList: TList);
begin
  with ReportItem do
  begin
    AList.Add(TObject(Integer(AbsoluteOrigin.X)));
    AList.Add(TObject(Integer(AbsoluteOrigin.X + Width)));
  end;
end;

procedure TdxPSCustomContainerItemDefinition.AddReportItemToDelimitersVert(AList: TList);
begin
  with ReportItem do
  begin
    AList.Add(TObject(Integer(AbsoluteOrigin.Y)));
    AList.Add(TObject(Integer(AbsoluteOrigin.Y + Height)));
  end;
end;

function TdxPSCustomContainerItemDefinition.GetSizeChangeReportItem: TdxReportVisualItem;
begin
  Result := ReportItem;
end;

function TdxPSCustomContainerItemDefinition.GetSizeMeasureReportItem: TdxReportVisualItem;
begin
  Result := ReportItem;
end;

function TdxPSCustomContainerItemDefinition.GetRootContainer: TWinControl;
begin
  Result := ReportLink.Container;
end;

function TdxPSCustomContainerItemDefinition.GetTopLevelRootContainer: TWinControl;
begin
  Result := ReportLink.TopLevelContainer;
end;

{ TdxPSContainerControlDefinition }

procedure TdxPSContainerControlDefinition.AddDelimitersHorz(AList: TList);
begin
  if OptionsPagination.Controls then AddReportItemToDelimitersHorz(AList);
end;

procedure TdxPSContainerControlDefinition.AddDelimitersVert(AList: TList);
begin
  if OptionsPagination.Controls then AddReportItemToDelimitersVert(AList);
end;

function TdxPSContainerControlDefinition.GetControl: TControl;
begin
  Result := TControl(Component);
end;

procedure TdxPSContainerControlDefinition.SetControl(Value: TControl);
begin
  Component := Value;
end;

{ TdxPSNativePrintableControlDefinition }

constructor TdxPSNativePrintableControlDefinition.Create(AReportLink: TdxCustomContainerReportLink;
  AComponent: TComponent; AReportItem: TdxReportVisualItem);
begin
  inherited;
  FDelimitersHorz := TList.Create;
  FDelimitersVert := TList.Create;
end;

destructor TdxPSNativePrintableControlDefinition.Destroy;
begin
  FreeAndNil(FDelimitersVert);
  FreeAndNil(FDelimitersHorz);
  inherited;
end;

procedure TdxPSNativePrintableControlDefinition.AddDelimitersHorz(AList: TList);
begin
  with OptionsPagination do
  begin
    if Controls then
      AddReportItemToDelimitersHorz(AList);
    if ControlDetails then
      dxAppendList(DelimitersHorz, AList);
  end
end;

procedure TdxPSNativePrintableControlDefinition.AddDelimitersVert(AList: TList);
begin
  with OptionsPagination do
  begin
    if Controls then
      AddReportItemToDelimitersVert(AList);
    if ControlDetails then
      dxAppendList(DelimitersVert, AList);
  end
end;

procedure TdxPSNativePrintableControlDefinition.GetData(AReportLink: TBasedxReportLink);
begin
  with TBasedxReportLinkAccess(AReportLink) do
  begin
    InternalGetDelimiters(DelimitersHorz, DelimitersVert);
    FReportDimension := Point(ReportWidth, ReportHeight);
  end;
  ShiftDelimiters;
end;

procedure TdxPSNativePrintableControlDefinition.ShiftDelimiters;
var
  Offset: TPoint;
begin
  Offset := DelimitersOffset;
  dxPSUtl.dxShiftIntegerListValues(DelimitersHorz, Offset.X);
  dxPSUtl.dxShiftIntegerListValues(DelimitersVert, Offset.Y);
end;

function TdxPSNativePrintableControlDefinition.GetSizeChangeReportItem: TdxReportVisualItem;
begin
  Result := ReportItem.Parent;
end;

function TdxPSNativePrintableControlDefinition.GetDelimitersOffset: TPoint;
var
  Parent: TWinControl;
begin
  Result := Point(Control.Left, Control.Top);
  Parent := Control.Parent;
  while Parent <> TopLevelRootContainer do
  begin
    Inc(Result.X, Parent.Left);
    Inc(Result.Y, Parent.Top);
    Parent := Parent.Parent;
  end;
end;

{ TdxPSContainerWinControlDefinition }

function TdxPSContainerWinControlDefinition.GetSizeChangeReportItem: TdxReportVisualItem;
begin
  Result := ReportItem.Parent;
end;

{ TdxPSContainerDefinition }

function TdxPSContainerDefinition.GetSizeMeasureReportItem: TdxReportVisualItem;
begin
  Result := ReportItem.Parent;
end;

{ TdxPSWinControlIterator }

constructor TdxPSWinControlIterator.Create(AControl: TWinControl);
begin
  inherited Create;
  FControl := AControl;
  FCounter := -1;
end;

class function TdxPSWinControlIterator.ContainerClass: TWinControlClass;
begin
  Result := TWinControl;
end;

function TdxPSWinControlIterator.GetControl(Index: Integer): TControl;
begin
  Result := Control.Controls[Index];
end;

function TdxPSWinControlIterator.GetControlCount: Integer;
begin
  Result := Control.ControlCount;
end;

procedure TdxPSWinControlIterator.GoBeforeBOF;
begin
  FCounter := -1;
end;

procedure TdxPSWinControlIterator.GoBeyondEOF;
begin
  FCounter := ControlCount;
end;

function TdxPSWinControlIterator.IsBOF: Boolean;
begin
  Result := FCounter < 1;
end;

function TdxPSWinControlIterator.IsEOF: Boolean;
begin
  Result := FCounter + 1 = ControlCount;
end;

function TdxPSWinControlIterator.Next: TControl;
begin
  if not IsEOF then
  begin
    Inc(FCounter);
    Result := Controls[FCounter];
  end
  else
    Result := nil;
end;

function TdxPSWinControlIterator.Prev: TControl;
begin
  if not IsBOF then
  begin
    Dec(FCounter);
    Result := Controls[FCounter];
  end
  else
    Result := nil;
end;

{ TdxPSCustomProducer }

constructor TdxPSCustomProducer.Create(AReportLink: TdxCustomContainerReportLink;
  AnObject: TComponent);
begin
  inherited Create;
  FReportLink := AReportLink;
  Initialize(AnObject);
end;

class function TdxPSCustomProducer.CanHasAvailableChildren: Boolean;
begin
  Result := False;
end;

class function TdxPSCustomProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := False;
end;

class function TdxPSCustomProducer.Reenterable: Boolean;
begin
  Result := True;
end;

function TdxPSCustomProducer.Definition: TdxPSCustomContainerItemDefinition;
begin
  Result := ReportLink.DefinitionsByContainerItem[ProducingObject];
end;

class function TdxPSCustomProducer.DefinitionClass: TdxPSCustomContainerItemDefinitionClass;
begin
  Result := TdxPSCustomContainerItemDefinition;
end;

procedure TdxPSCustomProducer.Initialize(AnObject: TComponent);
begin
  FProducingObject := AnObject;
end;

function TdxPSCustomProducer.ProducingObject: TComponent;
begin
  Result := FProducingObject;
end;

function TdxPSCustomProducer.ProducingObjectFriendlyName: string;
begin
  Result := ProducingObject.Name;
end;

function TdxPSCustomProducer.ReportLink: TdxCustomContainerReportLink;
begin
  Result := FReportLink;
end;

procedure TdxPSCustomProducer.Reposition;
begin
  DoReposition;
end;

function TdxPSCustomProducer.RootContainer: TWinControl;
begin
  Result := ReportLink.Container;
end;

function TdxPSCustomProducer.TopLevelRootContainer: TWinControl;
begin
  Result := ReportLink.TopLevelContainer;
end;

class procedure TdxPSCustomProducer.Register;
begin
  dxPSContainerItemProducerFactory.Register(Self);
end;

class procedure TdxPSCustomProducer.Unregister;
begin
  if not FIsContainerItemProducerFactoryReleased then
    dxPSContainerItemProducerFactory.Unregister(Self);
end;

class function TdxPSCustomProducer.HelperProducer: TdxPSCustomProducerClass;
begin
  Result := dxPSProducerHelperFactory[Self];
end;

class function TdxPSCustomProducer.BuddyClass: TdxPSCustomProducerClass;
begin
  Result := nil;
end;

function TdxPSCustomProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := True;
end;

function TdxPSCustomProducer.GetScrollPosLeft: Integer;
begin
  Result := 0;
end;

function TdxPSCustomProducer.GetScrollPosTop: Integer;
begin
  Result := 0;
end;

procedure TdxPSCustomProducer.GetImageLists(AProc: TdxPSGetImageListProc);
begin
end;

procedure TdxPSCustomProducer.InitializeOptionsPlace(AnOptions: TdxCustomContainerReportLinkOptionsItemPlace);
begin
  with AnOptions do
  begin
    ExpandHeight := ObjectExpandHeight;
    ExpandWidth := ObjectExpandWidth;
    ShrinkHeight := ObjectShrinkHeight;
    ShrinkWidth := ObjectShrinkWidth;
  end;
end;

procedure TdxPSCustomProducer.DoReposition;
var
  ItemHeight, ItemWidth: Integer;
begin
  with Definition do
  begin
    if OptionsPlace.HasVertResizing then
    begin
      ItemHeight := MeasureItemHeight(SizeMeasureReportItem);
      if OptionsPlace.ExpandHeight and (SizeChangeReportItem.Height < ItemHeight) then
        SizeChangeReportItem.Height := ItemHeight;
      if OptionsPlace.ShrinkHeight and (SizeChangeReportItem.Height > ItemHeight) and (ItemHeight <> 0) then
        SizeChangeReportItem.Height := ItemHeight;
    end;

    if OptionsPlace.HasHorzResizing then
    begin
      ItemWidth := MeasureItemWidth(SizeMeasureReportItem);
      if OptionsPlace.ExpandWidth and (SizeChangeReportItem.Width < ItemWidth) then
        SizeChangeReportItem.Width := ItemWidth;
      if OptionsPlace.ShrinkWidth and (SizeChangeReportItem.Width > ItemWidth) then
        SizeChangeReportItem.Width := ItemWidth;
    end;
  end;
end;

function TdxPSCustomProducer.MeasureItemHeight(AItem: TdxReportVisualItem): Integer;
begin
  Result := AItem.MeasureHeight(Canvas);
end;

function TdxPSCustomProducer.MeasureItemWidth(AItem: TdxReportVisualItem): Integer;
begin
  Result := AItem.MeasureWidth(Canvas);
end;

function TdxPSCustomProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

function TdxPSCustomProducer.ObjectExpandWidth: Boolean;
begin
  Result := False;
end;

function TdxPSCustomProducer.ObjectShrinkHeight: Boolean;
begin
  Result := False;
end;

function TdxPSCustomProducer.ObjectShrinkWidth: Boolean;
begin
  Result := False;
end;

function TdxPSCustomProducer.OptionsTransparent: TdxCustomContainerReportLinkOptionsTransparent;
begin
  Result := ReportLink.OptionsTransparent;
end;

function TdxPSCustomProducer.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

function TdxPSCustomProducer.GetDefinition(Component: TComponent): TdxPSCustomContainerItemDefinition;
begin
  Result := ReportLink.DefinitionsByContainerItem[Component];
end;

function TdxPSCustomProducer.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ReportLink.ComponentState;
end;

function TdxPSCustomProducer.GetProducer(Component: TComponent): TdxPSCustomProducer;
begin
  Result := ReportLink.Producers[Component];
end;

{ TdxPSCustomContainerItemProducer }

function TdxPSCustomContainerItemProducer.Control: TControl;
begin
  Result := ProducingObject;
end;

class function TdxPSCustomContainerItemProducer.ControlClass: TControlClass;
begin
  Result := TControl;
end;

function TdxPSCustomContainerItemProducer.Definition: TdxPSContainerControlDefinition;
begin
  Result := inherited Definition as TdxPSContainerControlDefinition;
end;

class function TdxPSCustomContainerItemProducer.DefinitionClass: TdxPSCustomContainerItemDefinitionClass;
begin
  Result := TdxPSContainerControlDefinition;
end;

class function TdxPSCustomContainerItemProducer.PairClass: TClass;
begin
  Result := ControlClass;
end;

function TdxPSCustomContainerItemProducer.ProducingObject: TControl;
begin
  Result := TControl(inherited ProducingObject);
end;

function TdxPSCustomContainerItemProducer.ProducingObjectFriendlyName: string;
begin
  Result := '';
  if not IsDesigning then
    Result := Control_GetText(Control);
  if Result = '' then
    Result := inherited ProducingObjectFriendlyName;
end;

function TdxPSCustomContainerItemProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := ((csAcceptsControls in Control_GetControlStyle(Control)) and
    ((AChildControl.Owner <> Control)) or
    (Control is TCustomForm) or (Control is TCustomFrame));
end;

function TdxPSCustomContainerItemProducer.CreateIterator: IdxPSContainerIterator;
begin
  Result := nil;
end;

function TdxPSCustomContainerItemProducer.GetContentColor: TColor;
begin
  if Control.Enabled then
    Result := Control_GetColor(Control)
  else
    Result := DefaultControlDisabledContentColor;
end;

function TdxPSCustomContainerItemProducer.GetControlBoundsRect: TRect;
begin
  Result := Control.ClientRect;
  OffsetRect(Result, Control.Left, Control.Top);
end;

function TdxPSCustomContainerItemProducer.GetFont: TFont;
begin
  Result := Control_GetFont(Control);
end;

function TdxPSCustomContainerItemProducer.GetFontColor: TColor;
begin
  if Control.Enabled then
    Result := Font.Color
  else
    Result := DefaultControlDisabledTextColor;
end;

function TdxPSCustomContainerItemProducer.GetFontIndex: Integer;
begin
  Result := ReportLink.GetPreparedFontIndex(Font, IsFontSubstitutable, FontName, FontColor, FontStyle, FontOrientation);
end;

function TdxPSCustomContainerItemProducer.GetFontName: string;
begin
  Result := Font.Name;
end;

function TdxPSCustomContainerItemProducer.GetFontOrientation: Integer;
begin
  Result := Font.Orientation;
end;

function TdxPSCustomContainerItemProducer.GetFontStyle: TFontStyles;
begin
  Result := Font.Style;
end;

function TdxPSCustomContainerItemProducer.IsFontSubstitutable: Boolean;
begin
  Result := True;
end;

function TdxPSCustomContainerItemProducer.HostClass: TdxReportCellClass;
begin
  Result := TdxReportCell;
end;

procedure TdxPSCustomContainerItemProducer.InitializeHost(ACell: TdxReportCell);
begin
  with ACell do
  begin
    BoundsRect := ControlBounds;
    Color := ContentColor;
    CellSides := [];
    ExcludeFromClipRgn := True;
    Transparent := True;
  end;
end;

procedure TdxPSCustomContainerItemProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  with AnItem do
  begin
    BoundsRect := GetControlBoundsRect;
    Color := ContentColor;
    Data := TdxNativeInt(TObject(Control));
    FontIndex := Self.FontIndex;
    Transparent := OptionsTransparent.Controls;
  end;
end;

function TdxPSCustomContainerItemProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TAbstractdxReportCellData;
end;

function TdxPSCustomContainerItemProducer.GetControlBounds: TRect;
begin
  Result := GetControlBoundsRect;
  if Control <> RootContainer then {.1}
    with Producers[Control.Parent] do
      OffsetRect(Result, ScrollPosLeft, ScrollPosTop);
end;

{ TdxPSCustomDelegateProducer }

procedure TdxPSCustomDelegateProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  Producer.InitializeItem(AnItem);
end;

function TdxPSCustomDelegateProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := Producer.ItemClass;
end;

{ TdxPSNativePrintableControlProducer }

class function TdxPSNativePrintableControlProducer.Reenterable: Boolean;
begin
  Result := False;
end;

function TdxPSNativePrintableControlProducer.Definition: TdxPSNativePrintableControlDefinition;
begin
  Result := inherited Definition as TdxPSNativePrintableControlDefinition;
end;

class function TdxPSNativePrintableControlProducer.DefinitionClass: TdxPSCustomContainerItemDefinitionClass;
begin
  Result := TdxPSNativePrintableControlDefinition;
end;

class function TdxPSNativePrintableControlProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := True;
end;

class procedure TdxPSNativePrintableControlProducer.Register;
begin
  dxPSNativePrintableControlProducerFactory.Register(Self);
end;

class procedure TdxPSNativePrintableControlProducer.Unregister;
begin
  if not FIsNativePrintableControlProducerFactoryReleased then
    dxPSNativePrintableControlProducerFactory.Unregister(Self);
end;

procedure TdxPSNativePrintableControlProducer.AdjustItemBounds(AnItem: TdxReportVisualItem);
var
  R: TRect;
begin
  with AnItem do
  begin
    R := BoundsRect;
    InflateRect(R, AnItem.BorderClass.Thickness - 1, AnItem.BorderClass.Thickness - 1);
    if ShowShadow then
    begin
      Inc(R.Right, ShadowDepth);
      Inc(R.Bottom, ShadowDepth);
    end;

    if not (Control is TWinControl) then
    begin
      OffsetRect(R, Control.Left, Control.Top);
      with Producers[Control.Parent] do
        OffsetRect(R, ScrollPosLeft, ScrollPosTop);
    end;

    BoundsRect := R;

    with Definition.OptionsPlace do
    begin
      if not ShrinkWidth and not ExpandWidth and (Width < Parent.Width) then
        Width := Parent.Width;
      if not ShrinkHeight and not ExpandHeight and (Height < Parent.Height) then
        Height := Parent.Height;
    end;

    if not Parent.ClipChildren then
    begin
      Parent.Height := Height;
      Parent.Width := Width;
    end;
  end;
end;

function TdxPSNativePrintableControlProducer.CreateControlReportLink(
  var AIsOuterLinkUsed: Boolean): TBasedxReportLink;
begin
  Result := ReportLink.DoGetReportLink(Control);
  AIsOuterLinkUsed := Result <> nil;
  if not AIsOuterLinkUsed then
    Result := GetLinkClass(Control.ClassType).Create(nil);
  InitializeReportLink(Result);
end;

procedure TdxPSNativePrintableControlProducer.CreateNativePrintableControlData(
  AItem: TdxReportVisualItem);
var
  ALink: TBasedxReportLinkAccess;
  APoint: TPoint;
begin
  ALink := TBasedxReportLinkAccess(CreateControlReportLink(FIsOuterLinkUsed));
  try
    if ALink.DataProviderPresent then
    begin
      if not IsOuterLinkUsed then
        ALink.RestoreFromOriginal;
      ALink.RebuildReport;
      AItem.Assign(ALink.ReportCells.Cells);
      APoint := TopLevelRootContainer.ScreenToClient(Control.ClientToScreen(cxNullPoint));
      AItem.ReportCells.AppendOverlays(ALink.ReportCells, APoint.X, APoint.Y);
      Definition.GetData(ALink);
    end
    else
      AItem.CellSides := [];
  finally
    DeinitializeReportLink(ALink);
    if not IsOuterLinkUsed then
      FreeAndNil(ALink);
  end;
end;

procedure TdxPSNativePrintableControlProducer.DeinitializeReportLink(AControlReportLink: TBasedxReportLink);
begin
  with TBasedxReportLinkAccess(AControlReportLink) do
  begin
    if ComponentPrinter = InternalComponentPrinter then
      ComponentPrinter := nil;
    Controller := nil;
    ReportLink.DoInitializeReportLink(AControlReportLink, psAfter);
  end;
end;

class function TdxPSNativePrintableControlProducer.GetLinkClass(AClass: TClass): TdxReportLinkClass;
begin
  Result := dxPSLinkClassByCompClass(TComponentClass(AClass));
end;

procedure TdxPSNativePrintableControlProducer.InitializeReportLink(AControlReportLink: TBasedxReportLink);
begin
  with TBasedxReportLinkAccess(AControlReportLink) do
  begin
    if ComponentPrinter = nil then
      ComponentPrinter := InternalComponentPrinter;
    Controller := ReportLink;
    Component := Control;
    ReportLink.DoInitializeReportLink(AControlReportLink, psBefore);
  end;
end;

class function TdxPSNativePrintableControlProducer.HasReportLink(AComponent: TComponent): Boolean;
var
  LinkClass: TdxReportLinkClass;
begin
  LinkClass := GetLinkClass(AComponent.ClassType);
  Result := (LinkClass <> nil) and LinkClass.Aggregable;
end;

function TdxPSNativePrintableControlProducer.HostClass: TdxReportCellClass;
begin
  Result := TdxReportNativePrintableControlHost;
end;

procedure TdxPSNativePrintableControlProducer.InitializeHost(ACell: TdxReportCell);
begin
  inherited;
  ACell.ClipChildren := not (ObjectExpandHeight and ObjectExpandWidth);
end;

procedure TdxPSNativePrintableControlProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  CreateNativePrintableControlData(AnItem);
  InitializeNativePrintableControlHost(AnItem);
end;

procedure TdxPSNativePrintableControlProducer.InitializeNativePrintableControlHost(AnItem: TdxReportVisualItem);
begin
  AnItem.Data := TdxNativeInt(TObject(Control)); // should be copied as well !!!
  AnItem.ExcludeFromClipRgn := True;
  AnItem.Transparent := True;
  AnItem.CellSides := [];  //v.3.2

  AdjustItemBounds(AnItem);
end;

function TdxPSNativePrintableControlProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCell;
end;

function TdxPSNativePrintableControlProducer.MeasureItemHeight(AnItem: TdxReportVisualItem): Integer;
begin
  Result := Definition.ReportDimension.Y;
end;

function TdxPSNativePrintableControlProducer.MeasureItemWidth(AnItem: TdxReportVisualItem): Integer;
begin
  Result := Definition.ReportDimension.X;
end;

function TdxPSNativePrintableControlProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

function TdxPSNativePrintableControlProducer.ObjectExpandWidth: Boolean;
begin
  Result := False;
end;

{ TdxPSContainerControlProducer }

procedure TdxPSContainerControlProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.BoundsRect := GetControlBoundsRect;
  AnItem.CellSides := csAll;
  //AnItem.ExcludeFromClipRgn := True;
end;

{ TdxPSControlAsMetafileProducer }

class function TdxPSControlAsMetafileProducer.PairClass: TClass;
begin
  Result := BuddyClass;
end;

class procedure TdxPSControlAsMetafileProducer.Register;
begin
  dxPSProducerHelperFactory.Register(Self);
end;

class procedure TdxPSControlAsMetafileProducer.Unregister;
begin
  dxPSProducerHelperFactory.Unregister(Self);
end;

procedure TdxPSControlAsMetafileProducer.Reposition;
begin
end;

class function TdxPSControlAsMetafileProducer.BuddyClass: TdxPSCustomProducerClass;
begin
  Result := TdxPSContainerControlProducer;
end;

function TdxPSControlAsMetafileProducer.CreateControlImage(AItem: TdxReportVisualItem): TGraphic;
var
  AMetaFileCanvas: TMetafileCanvas;
begin
  Result := TdxReportCellGraphic(AItem).CreateImage(TMetafile);
  try
    Result.Height := AItem.Height;
    Result.Width := AItem.Width;
    AMetaFileCanvas := TMetafileCanvas.Create(TMetafile(Result), 0);
    try
      Control.Perform(WM_PAINT, AMetaFileCanvas.Handle, 0)
    finally
      AMetaFileCanvas.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TdxPSControlAsMetafileProducer.InitializeItem(AItem: TdxReportVisualItem);
begin
  inherited InitializeItem(AItem);
  TdxReportCellGraphic(AItem).CellSides := [];
  TdxReportCellGraphic(AItem).ImageTransparent := False;
  CreateControlImage(AItem);
end;

function TdxPSControlAsMetafileProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellGraphic;
end;

{ TdxPSBevelProducer }

function TdxPSBevelProducer.Control: TBevel;
begin
  Result := inherited Control as TBevel;
end;

class function TdxPSBevelProducer.ControlClass: TControlClass;
begin
  Result := TBevel;
end;

procedure TdxPSBevelProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.CellSides := BevelShapeMap[Control.Shape];
  AnItem.BorderClass := BevelStyleMap[Control.Style, Control.Shape];
  AnItem.Transparent := True;
end;

function TdxPSBevelProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellBox;
end;

{ TdxPSPaintBoxProducer }

function TdxPSPaintBoxProducer.Control: TPaintBox;
begin
  Result := inherited Control as TPaintBox;
end;

class function TdxPSPaintBoxProducer.ControlClass: TControlClass;
begin
  Result := TPaintBox;
end;

procedure TdxPSPaintBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.CellSides := [];
  AnItem.BorderClass := TdxPSCellNullBorder;
  AnItem.Transparent := True;
end;

function TdxPSPaintBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellBox;
end;

{ TdxPSShapeProducer }

function TdxPSShapeProducer.Control: TShape;
begin
  Result := inherited Control as TShape;
end;

class function TdxPSShapeProducer.ControlClass: TControlClass;
begin
  Result := TShape;
end;

procedure TdxPSShapeProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  V: Integer;
begin
  inherited;
  with TCustomdxReportCellShape(AnItem) do
  begin
    CellSides := [];
    ContentBkColor := Control_GetColor(Control.Parent);
    ContentPattern := BrushStyleMap[Control.Brush.Style];
    ShapeBorderColor := Control.Pen.Color;
    ShapeBorderThickness := Control.Pen.Width;
    if (Control.Pen.Style <> psSolid) and (ShapeBorderThickness > 1) then
      ShapeBorderThickness := 1;
    ShapeColor := Control.Brush.Color;
    if Control.Pen.Style = psClear then
      ShapeBorderColor := ShapeColor;
    ShapeTransparent := Control.Brush.Style = bsClear;
    if Control.Shape in [stRoundRect, stRoundSquare] then
    begin
      if ShapeHeight > ShapeWidth then
        V := ShapeWidth
      else
        V := ShapeHeight;
      TdxReportCellRoundRect(AnItem).EllipseHeight := V div 4;
      TdxReportCellRoundRect(AnItem).EllipseWidth :=  V div 4;
    end;
    Transparent := True;
  end;
end;

function TdxPSShapeProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := ShapeTypeMap[Control.Shape];
end;

{ TdxPSCustomLabelProducer }

function TdxPSCustomLabelProducer.Control: TCustomLabel;
begin
  Result := inherited Control as TCustomLabel;
end;

class function TdxPSCustomLabelProducer.ControlClass: TControlClass;
begin
  Result := TCustomLabel;
end;

function TdxPSCustomLabelProducer.GetControlBoundsRect: TRect;
begin
  Result := inherited GetControlBoundsRect;
  OffsetRect(Result, 0, -2);
end;

procedure TdxPSCustomLabelProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited InitializeItem(AnItem);
  with TdxReportCellString(AnItem) do
  begin
    CellSides := [];
    Multiline := Label_GetWordWrap(Control);
    HidePrefix := True;
    Text := Label_GetText(Control);
    TextAlignX := dxPSCore.dxTextAlignX[Label_GetAlignment(Control)];
    TextAlignY := dxPSCore.dxTextAlignY[Label_GetLayout(Control)];
    AdjustFont := Label_GetAutoSize(Control);
    EndEllipsis := TCustomLabelAccess(Control).EllipsisPosition <> epNone;
  end;
end;

function TdxPSCustomLabelProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellString;
end;

function TdxPSCustomLabelProducer.ObjectExpandHeight: Boolean;
begin
  Result := ReportLink.OptionsBehavior.LabelAutoHeight;
end;

function TdxPSCustomLabelProducer.ObjectExpandWidth: Boolean;
begin
  Result := Label_GetAutoSize(Control);
end;

{ TdxPSContainerCustomWinControlProducer }

function TdxPSContainerCustomWinControlProducer.Control: TWinControl;
begin
  Result := inherited Control as TWinControl;
end;

class function TdxPSContainerCustomWinControlProducer.ControlClass: TControlClass;
begin
  Result := TWinControl;
end;

function TdxPSContainerCustomWinControlProducer.CreateIterator: IdxPSContainerIterator;
begin
  IteratorClass.Create(Control).GetInterface(IdxPSContainerIterator, Result);
end;

class function TdxPSContainerCustomWinControlProducer.DefinitionClass: TdxPSCustomContainerItemDefinitionClass;
begin
  Result := TdxPSContainerWinControlDefinition;
end;

function TdxPSContainerCustomWinControlProducer.HostClass: TdxReportCellClass;
begin
  Result := TdxReportWinControlHost;
end;

procedure TdxPSContainerCustomWinControlProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.Left := 0;
  AnItem.Top := 0;
  AnItem.BorderClass := Ctl3DBorderClassMap[Control_GetCtl3D(Control)];
  //AnItem.ExcludeFromClipRgn := True;
end;

class function TdxPSContainerCustomWinControlProducer.IteratorClass: TdxPSWinControlIteratorClass;
begin
  Result := TdxPSWinControlIterator;
end;

{ TdxPSWinControlAsMetafileProducer }

class function TdxPSWinControlAsMetafileProducer.PairClass: TClass;
begin
  Result := BuddyClass;
end;

procedure TdxPSWinControlAsMetafileProducer.Reposition;
begin
end;

class procedure TdxPSWinControlAsMetafileProducer.Register;
begin
  dxPSProducerHelperFactory.Register(Self);
end;

class procedure TdxPSWinControlAsMetafileProducer.Unregister;
begin
  dxPSProducerHelperFactory.Unregister(Self);
end;

class function TdxPSWinControlAsMetafileProducer.BuddyClass: TdxPSCustomProducerClass;
begin
  Result := TdxPSContainerWinControlProducer;
end;

function TdxPSWinControlAsMetafileProducer.CreateControlImage(AItem: TdxReportVisualItem): TGraphic;
var
  ACanvas: TMetaFileCanvas;
begin
  Result := TdxReportCellGraphic(AItem).CreateImage(TMetafile);
  Result.Width := AItem.Width;
  Result.Height := AItem.Height;
  ACanvas := TMetaFileCanvas.Create(TMetafile(Result), NullDC);
  try
    Control.PaintTo(ACanvas.Handle, 0, 0);
  finally
    ACanvas.Free;
  end;
end;

procedure TdxPSWinControlAsMetafileProducer.InitializeItem(AItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellGraphic(AItem).CellSides := [];
  TdxReportCellGraphic(AItem).ImageTransparent := False;
  CreateControlImage(AItem);
end;

function TdxPSWinControlAsMetafileProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellGraphic;
end;

{ TdxPSContainerWinControlProducer }

procedure TdxPSContainerWinControlProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).Text := Control_GetText(Control);
end;

function TdxPSContainerWinControlProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellString;
end;

function TdxPSContainerWinControlProducer.ObjectExpandHeight: Boolean;
begin
  Result := True;
end;

{ TdxPSDateTimePickerProducer }

function TdxPSDateTimePickerProducer.Control: TDateTimePicker;
begin
  Result := inherited Control as TDateTimePicker;
end;

class function TdxPSDateTimePickerProducer.ControlClass: TControlClass;
begin
  Result := TDateTimePicker;
end;

procedure TdxPSDateTimePickerProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  with TdxReportCellCheck(AnItem) do
  begin
    //AnItem.BorderClass := Ctl3DBorderClassMap[ControlGetCtl3D(Control)];
    if Control.ShowCheckbox then
    begin
      ButtonEdgeStyle := cbes3D;
      Checked := Control.Checked;
      CheckPos := ccpLeft;
    end;
  end;
end;

function TdxPSDateTimePickerProducer.ItemClass: TdxReportVisualItemClass;
const
  ItemClasses: array[Boolean] of TdxReportVisualItemClass = (TdxReportCellString, TdxReportCellCheck);
begin
  Result := ItemClasses[Control.ShowCheckbox];
end;

{ TdxPSCustomHotKeyProducer }

function TdxPSCustomHotKeyProducer.Control: TCustomHotKey;
begin
  Result := inherited Control as TCustomHotKey;
end;

class function TdxPSCustomHotKeyProducer.ControlClass: TControlClass;
begin
  Result := TCustomHotKey;
end;

procedure TdxPSCustomHotKeyProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).Text := Menus.ShortCutToText(HotKey_GetHotKey(Control));
end;

{ TdxPSCustomStaticTextProducer }

function TdxPSCustomStaticTextProducer.Control: TCustomStaticText;
begin
  Result := inherited Control as TCustomStaticText;
end;

class function TdxPSCustomStaticTextProducer.ControlClass: TControlClass;
begin
  Result := TCustomStaticText;
end;

procedure TdxPSCustomStaticTextProducer.InitializeItem(AnItem: TdxReportVisualItem);
const
  BorderClassMap: array[TStaticBorderStyle] of TdxPSCellBorderClass =
    (TdxPSCellNullBorder, TdxPSCellUltraFlatBorder, TdxPSCellSunkenSoftBorder);
  CellSidesMap: array[TStaticBorderStyle] of TdxCellSides = ([], csAll, csAll);
begin
  inherited;
  TdxReportCellString(AnItem).BorderClass := BorderClassMap[StaticText_GetBorderStyle(Control)];
  TdxReportCellString(AnItem).CellSides := CellSidesMap[StaticText_GetBorderStyle(Control)];
  TdxReportCellString(AnItem).HidePrefix := True;
  TdxReportCellString(AnItem).TextAlignX := dxPSCore.dxTextAlignX[StaticText_GetAlignment(Control)];
end;

{ TdxPSCustomEditProducer }

function TdxPSCustomEditProducer.Control: TCustomEdit;
begin
  Result := inherited Control as TCustomEdit;
end;

class function TdxPSCustomEditProducer.ControlClass: TControlClass;
begin
  Result := TCustomEdit;
end;

procedure TdxPSCustomEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).CellSides := BorderStyleMap[Edit_GetBorderStyle(Control)];
  //TdxReportCellString(AnItem).BorderClass := Ctl3DBorderClassMap[ControlGetCtl3D(Control)];
end;

{ TdxPSCustomMemoProducer }

function TdxPSCustomMemoProducer.Control: TCustomMemo;
begin
  Result := inherited Control as TCustomMemo;
end;

class function TdxPSCustomMemoProducer.ControlClass: TControlClass;
begin
  Result := TCustomMemo;
end;

function TdxPSCustomMemoProducer.ProducingObjectFriendlyName: string;
var
  Lines: TStrings;
begin
  Result := '';
  Lines := Memo_GetLines(Control);
  if not IsDesigning and (Lines.Count <> 0) then
    Result := dxPSMakeFriendlyNameFromStrings(Lines);
  if Result = '' then
    Result := inherited ProducingObjectFriendlyName;
end;

procedure TdxPSCustomMemoProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).TextAlignX := dxPSCore.dxTextAlignX[Memo_GetAlignment(Control)];
  TdxReportCellString(AnItem).TextAlignY := taTop;
  TdxReportCellString(AnItem).Multiline := True;
end;

function TdxPSCustomMemoProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

{ TdxPSCustomComboBoxProducer }

function TdxPSCustomComboBoxProducer.Control: TCustomComboBox;
begin
  Result := inherited Control as TCustomComboBox;
end;

class function TdxPSCustomComboBoxProducer.ControlClass: TControlClass;
begin
  Result := TCustomComboBox;
end;

procedure TdxPSCustomComboBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  //TdxReportCellString(AnItem).BorderClass := Ctl3DBorderClassMap[ControlGetCtl3D(Control)];
   TdxReportCellString(AnItem).Text := ComboBox_GetCurrentText(Control);
end;

{ TdxPSComboBoxExProducer }

function TdxPSCustomComboBoxExProducer.Control: TCustomComboBoxEx;
begin
  Result := inherited Control as TCustomComboBoxEx;
end;

class function TdxPSCustomComboBoxExProducer.ControlClass: TControlClass;
begin
  Result := TCustomComboBoxEx;
end;

procedure TdxPSCustomComboBoxExProducer.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(Control.Images);
end;

procedure TdxPSCustomComboBoxExProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  with TdxReportCellImage(AnItem) do
  begin
    if Control.ItemIndex <> -1 then
      ImageIndex := Control.ItemsEx[Control.ItemIndex].ImageIndex;
    ImageList := Control.Images;
    MakeSpaceForEmptyImage := True;
    if Control.Style in [csExSimple, csExDropDown] then
      Text := Control_GetText(Control)
    else
      if Control.ItemIndex <> -1 then
        Text := Control.ItemsEx[Control.ItemIndex].Caption
      else
        Text := '';
  end;
end;

function TdxPSCustomComboBoxExProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellImage;
end;

function TdxPSCustomCheckBoxProducer.Control: TCustomCheckBox;
begin
  Result := inherited Control as TCustomCheckBox;
end;

class function TdxPSCustomCheckBoxProducer.ControlClass: TControlClass;
begin
  Result := TCustomCheckBox;
end;

procedure TdxPSCustomCheckBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  CheckState: TCheckBoxState;
begin
  inherited;
  CheckState := CheckBox_GetState(Control);
  with TdxReportCellCheck(AnItem) do
  begin
    ButtonEdgeStyle := cbes3D;
    CellSides := [];
    Checked := CheckState <> cbUnchecked;
    CheckPos := CheckAlignmentMap[CheckBox_GetAlignment(Control)];
    Enabled := CheckState <> cbGrayed;
    HidePrefix := True;
    TextAlignX := taLeft;
  end;
end;

function TdxPSCustomCheckBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellCheck;
end;

{ TdxPSRadioButtonProducer }

function TdxPSRadioButtonProducer.Control: TRadioButton;
begin
  Result := inherited Control as TRadioButton;
end;

class function TdxPSRadioButtonProducer.ControlClass: TControlClass;
begin
  Result := TRadioButton;
end;

procedure TdxPSRadioButtonProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  with TdxReportCellRadio(AnItem) do
  begin
    ButtonEdgeStyle := cbes3D;
    CellSides := [];
    Checked := Control.Checked;
    CheckPos := CheckAlignmentMap[Control.Alignment];
    Enabled := Control.Enabled;
    HidePrefix := True;
    TextAlignX := taLeft;
  end;
end;

function TdxPSRadioButtonProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellRadio;
end;

{ TdxPSCustomContainerProducer }

class function TdxPSCustomContainerProducer.CanHasAvailableChildren: Boolean;
begin
  Result := True;
end;

class function TdxPSCustomContainerProducer.DefinitionClass: TdxPSCustomContainerItemDefinitionClass;
begin
  Result := TdxPSContainerDefinition;
end;

procedure TdxPSCustomContainerProducer.InitializeItem(AnItem: TdxReportVisualItem);
const
  CellsSidesMap: array[Boolean] of TdxCellSides = ([], csAll);
begin
  inherited;
  //AnItem.ExcludeFromClipRgn := Control <> RootContainer;
  if Control = RootContainer then
  begin
    AnItem.CellSides := CellsSidesMap[ReportLink.OptionsRefinements.RootBorders];
    AnItem.Transparent := OptionsTransparent.Root;
  end
  else
    AnItem.Transparent := OptionsTransparent.Containers;
end;

function TdxPSCustomContainerProducer.ObjectExpandHeight: Boolean;
begin
  if Control = RootContainer then
    Result := True
  else
    Result := inherited ObjectExpandHeight;
end;

function TdxPSCustomContainerProducer.ObjectExpandWidth: Boolean;
begin
  if Control = RootContainer then
    Result := True
  else
    Result := inherited ObjectExpandWidth;
end;

function TdxPSCustomContainerProducer.ObjectShrinkHeight: Boolean;
begin
  if Control = RootContainer then
    Result := True
  else
    Result := inherited ObjectShrinkHeight;
end;

function TdxPSCustomContainerProducer.ObjectShrinkWidth: Boolean;
begin
  if Control = RootContainer then
    Result := True
  else
    Result := inherited ObjectShrinkWidth;
end;

{ TdxPSCustomPanelProducer }

function TdxPSCustomPanelProducer.Control: TCustomPanel;
begin
  Result := inherited Control as TCustomPanel;
end;

class function TdxPSCustomPanelProducer.ControlClass: TControlClass;
begin
  Result := TCustomPanel;
end;

class function TdxPSCustomPanelProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := True;
end;

procedure TdxPSCustomPanelProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  BevelInner, BevelOuter: TPanelBevel;
begin
  inherited;
  if Panel_GetBorderStyle(Control) <> bsSingle then
  begin
    BevelInner := Panel_GetBevelInner(Control);
    BevelOuter := Panel_GetBevelOuter(Control);
    if (BevelInner = bvNone) and (BevelOuter = bvNone) then
      TdxReportCellString(AnItem).CellSides := []
    else
      TdxReportCellString(AnItem).BorderClass := PanelBevelsMap[BevelInner, BevelOuter];
  end
  else
    TdxReportCellString(AnItem).BorderClass := PanelSingleBorderMap[Control_GetCtl3D(Control)];

  TdxReportCellString(AnItem).Text := Control_GetText(Control);
  TdxReportCellString(AnItem).TextAlignX := dxPSCore.dxTextAlignX[Panel_GetAlignment(Control)];
  TdxReportCellString(AnItem).TextAlignY := taCenterY;
end;

function TdxPSCustomPanelProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellString;
end;

function TdxPSCustomPanelProducer.ObjectExpandHeight: Boolean;
begin
  if Control <> RootContainer then
    Result := True
  else
    Result := inherited ObjectExpandHeight;
end;

function TdxPSCustomPanelProducer.ObjectExpandWidth: Boolean;
begin
  if Control <> RootContainer then
    Result := True
  else
    Result := inherited ObjectExpandWidth;
end;

{ TdxPSCustomGroupBoxProducer }

function TdxPSCustomGroupBoxProducer.Control: TCustomGroupBox;
begin
  Result := inherited Control as TCustomGroupBox;
end;

class function TdxPSCustomGroupBoxProducer.ControlClass: TControlClass;
begin
  Result := TCustomGroupBox;
end;

class function TdxPSCustomGroupBoxProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := True;
end;

procedure TdxPSCustomGroupBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
const
  BorderClasses: array[Boolean] of TdxPSCellBorderClass = (TdxPSCellUltraFlatBorder, TdxPSCellEtchedBorder);
begin
  inherited;
  with TdxReportGroup(AnItem) do
  begin
    BorderClass := BorderClasses[Control_GetCtl3D(Control)];
    BorderColor := dxPSCore.dxDefaultGridLineColor;
    CaptionAlignment := taLeft;
    CaptionText := Control_GetText(Control);
    CaptionTransparent := Transparent;
    CellSides := csAll;
    ShowCaption := True;
    UseOwnBorderClass := True;

    LookAndFeel := ReportLink.CreateGroupLookAndFeel(LookAndFeelClass);
    InitializeLookAndFeel(AnItem, LookAndFeel);
    LookAndFeel.Prepare(Canvas);
    CalculateCaptionTextWidth(Canvas);
  end;
end;

procedure TdxPSCustomGroupBoxProducer.InitializeLookAndFeel(AnItem: TdxReportVisualItem;
  ALookAndFeel: TdxPSReportGroupLookAndFeel);
begin
  ALookAndFeel.FontIndex := AnItem.FontIndex;
  ALookAndFeel.CaptionFontIndex := AnItem.FontIndex;
  ALookAndFeel.Color := ContentColor;
end;

function TdxPSCustomGroupBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportGroup;
end;

class function TdxPSCustomGroupBoxProducer.LookAndFeelClass: TdxPSReportGroupLookAndFeelClass;
begin
  Result := TdxPSReportGroupStandardLookAndFeel;
end;

{ TdxPSCustomRadioGroupProducer }

function TdxPSCustomRadioGroupProducer.Control: TCustomRadioGroup;
begin
  Result := inherited Control as TCustomRadioGroup;
end;

class function TdxPSCustomRadioGroupProducer.ControlClass: TControlClass;
begin
  Result := TCustomRadioGroup;
end;

function TdxPSCustomRadioGroupProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := not AChildControl.ClassNameIs('TGroupButton'); // RadioGroup's buttons are processed inside
end;

procedure TdxPSCustomRadioGroupProducer.CreateItems(AReportRadioGroup: TdxReportRadioGroup);
var
  Items: TStrings;
  I: Integer;
  Item: TdxCustomReportCellRadio;
begin
  Items := RadioGroup_GetItems(Control);
  for I := 0 to Items.Count - 1 do
  begin
    Item := AReportRadioGroup.Add(Items[I]);
    InitializeRadioItem(Item, I);
  end;
end;

procedure TdxPSCustomRadioGroupProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  with TdxReportRadioGroup(AnItem) do
  begin
    ButtonEdgeStyle := cbes3D;
    CheckPos := ccpLeft;
    ColumnCount := RadioGroup_GetColumns(Control);
    CreateItems(TdxReportRadioGroup(AnItem));
    ItemIndex := RadioGroup_GetItemIndex(Control);
    AdjustContent(Canvas);
  end;
end;

procedure TdxPSCustomRadioGroupProducer.InitializeRadioItem(AnItem: TdxCustomReportCellRadio; AnIndex: Integer);
begin
  AnItem.Enabled := Control.Controls[AnIndex].Enabled;
end;

function TdxPSCustomRadioGroupProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportRadioGroup;
end;

{ TdxPSRootContainerProducer }

procedure TdxPSRootContainerProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.Left := 0;
  AnItem.Top := 0;
end;

{ TdxPSPageControlIterator }

function TdxPSPageControlIterator.GetControl(Index: Integer): TControl;
begin
  Result := PageControl.ActivePage;
end;

function TdxPSPageControlIterator.GetControlCount: Integer;
begin
  Result := Ord(PageControl.PageCount > 0);
end;

function TdxPSPageControlIterator.GetPageControl: TPageControl;
begin
  Result := TPageControl(Control);
end;

{ TdxPSPageControlProducer }

function TdxPSPageControlProducer.Control: TPageControl;
begin
  Result := inherited Control as TPageControl;
end;

class function TdxPSPageControlProducer.ControlClass: TControlClass;
begin
  Result := TPageControl;
end;

function TdxPSPageControlProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := AChildControl = Control.ActivePage;
end;

class function TdxPSPageControlProducer.IteratorClass: TdxPSWinControlIteratorClass;
begin
  Result := TdxPSPageControlIterator;
end;

{ TdxPSTabSheetProducer }

function TdxPSTabSheetProducer.Control: TTabSheet;
begin
  Result := inherited Control as TTabSheet;
end;

class function TdxPSTabSheetProducer.ControlClass: TControlClass;
begin
  Result := TTabSheet;
end;

{ TdxPSTabControlProducer }

function TdxPSTabControlProducer.Control: TTabControl;
begin
  Result := inherited Control as TTabControl;
end;

class function TdxPSTabControlProducer.ControlClass: TControlClass;
begin
  Result := TTabControl;
end;

{ TdxPSNotebookIterator }

function TdxPSNotebookIterator.GetControl(Index: Integer): TControl;
begin
  with Notebook do
    Result := TPage(Pages.Objects[PageIndex]);
end;

function TdxPSNotebookIterator.GetControlCount: Integer;
begin
  Result := Ord(Notebook.Pages.Count <> 0);
end;

function TdxPSNotebookIterator.GetNotebook: TNotebook;
begin
  Result := TNotebook(Control);
end;

{ TdxPSNotebookPageProducer }

function TdxPSNotebookPageProducer.Control: TPage;
begin
  Result := inherited Control as TPage;
end;

class function TdxPSNotebookPageProducer.ControlClass: TControlClass;
begin
  Result := TPage;
end;

{ TdxPSNotebookProducer }

function TdxPSNotebookProducer.Control: TNotebook;
begin
  Result := inherited Control as TNotebook;
end;

class function TdxPSNotebookProducer.ControlClass: TControlClass;
begin
  Result := TNotebook;
end;

function TdxPSNotebookProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := AChildControl = Control.Pages.Objects[Control.PageIndex];
end;

class function TdxPSNotebookProducer.IteratorClass: TdxPSWinControlIteratorClass;
begin
  Result := TdxPSNotebookIterator;
end;

{ TdxPSTabbedNotebookIterator }

function TdxPSTabbedNotebookIterator.GetControl(Index: Integer): TControl;
begin
  with TabbedNotebook do
    Result := TTabPage(Pages.Objects[PageIndex]);
end;

function TdxPSTabbedNotebookIterator.GetControlCount: Integer;
begin
  Result := Ord(TabbedNotebook.Pages.Count <> 0);
end;

function TdxPSTabbedNotebookIterator.GetTabbedNotebook: TTabbedNotebook;
begin
  Result := TTabbedNotebook(Control);
end;

{ TdxPSTabbedNotebookPageProducer }

function TdxPSTabbedNotebookPageProducer.Control: TTabPage;
begin
  Result := inherited Control as TTabPage;
end;

class function TdxPSTabbedNotebookPageProducer.ControlClass: TControlClass;
begin
  Result := TTabPage;
end;

{ TdxPSTabbedNotebookProducer }

function TdxPSTabbedNotebookProducer.Control: TTabbedNotebook;
begin
  Result := inherited Control as TTabbedNotebook;
end;

class function TdxPSTabbedNotebookProducer.ControlClass: TControlClass;
begin
  Result := TTabbedNotebook;
end;

function TdxPSTabbedNotebookProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := AChildControl = Control.Pages.Objects[Control.PageIndex];
end;

class function TdxPSTabbedNotebookProducer.IteratorClass: TdxPSWinControlIteratorClass;
begin
  Result := TdxPSTabbedNotebookIterator;
end;

{ TdxPSScrollingWinControlProducer }

function TdxPSScrollingWinControlProducer.Control: TScrollingWinControl;
begin
  Result := inherited Control as TScrollingWinControl;
end;

class function TdxPSScrollingWinControlProducer.ControlClass: TControlClass;
begin
  Result := TScrollingWinControl;
end;

function TdxPSScrollingWinControlProducer.GetScrollBarPos(AScrollBar: TControlScrollBar): Integer;
begin
  if AScrollBar.Visible then
    Result := AScrollBar.Position
  else
    Result := 0;
end;

function TdxPSScrollingWinControlProducer.GetScrollPosLeft: Integer;
begin
  Result := GetScrollBarPos(Control.HorzScrollBar);
end;

function TdxPSScrollingWinControlProducer.GetScrollPosTop: Integer;
begin
  Result := GetScrollBarPos(Control.VertScrollBar);
end;

procedure TdxPSScrollingWinControlProducer.InitializeHost(ACell: TdxReportCell);

  function GetScrollValue(AValue, AMinValue: Integer): Integer;
  begin
    if AValue > AMinValue then
      Result := AValue
    else
      Result := AMinValue;
  end;

begin
  inherited;
  with Control do
  begin
    ACell.Height := GetScrollValue(VertScrollBar.Range, Height);
    ACell.Width := GetScrollValue(HorzScrollBar.Range, Width);
  end
end;

function TdxPSScrollingWinControlProducer.ObjectExpandHeight: Boolean;
begin
  Result := True;
end;

function TdxPSScrollingWinControlProducer.ObjectExpandWidth: Boolean;
begin
  Result := True;
end;

{ TdxPScxScrollBoxProducer }

function TdxPScxScrollBoxProducer.Control: TcxCustomScrollBox;
begin
  Result := inherited Control as TcxCustomScrollBox;
end;

class function TdxPScxScrollBoxProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomScrollBox;
end;

function TdxPScxScrollBoxProducer.GetScrollBarPos(AScrollBar: TcxScrollBoxScrollBarOptions): Integer;
begin
  if AScrollBar.Visible then
    Result := AScrollBar.Position
  else
    Result := 0;
end;

function TdxPScxScrollBoxProducer.GetScrollPosLeft: Integer;
begin
  Result := GetScrollBarPos(Control.HorzScrollBar);
end;

function TdxPScxScrollBoxProducer.GetScrollPosTop: Integer;
begin
  Result := GetScrollBarPos(Control.VertScrollBar);
end;

procedure TdxPScxScrollBoxProducer.InitializeHost(ACell: TdxReportCell);

  function GetScrollValue(AValue, AMinValue: Integer): Integer;
  begin
    if AValue > AMinValue then
      Result := AValue
    else
      Result := AMinValue;
  end;

begin
  inherited;
  with Control do
  begin
    ACell.Height := GetScrollValue(VertScrollBar.Range, Height);
    ACell.Width := GetScrollValue(HorzScrollBar.Range, Width);
  end;
end;

function TdxPScxScrollBoxProducer.ObjectExpandHeight: Boolean;
begin
  Result := True;
end;

function TdxPScxScrollBoxProducer.ObjectExpandWidth: Boolean;
begin
  Result := True;
end;

{ TdxPSCustomFrameProducer }

function TdxPSCustomFrameProducer.Control: TCustomFrame;
begin
  Result := inherited Control as TCustomFrame;
end;

class function TdxPSCustomFrameProducer.ControlClass: TControlClass;
begin
  Result := TCustomFrame;
end;

{ TdxPSCustomFormProducer }

function TdxPSCustomFormProducer.Control: TCustomForm;
begin
  Result := inherited Control as TCustomForm;
end;

class function TdxPSCustomFormProducer.ControlClass: TControlClass;
begin
  Result := TCustomForm;
end;

{ TdxPSContainerBuilder }

constructor TdxPSContainerBuilder.Create(AReportLink: TdxCustomContainerReportLink;
  AContainer: TWinControl; AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil);
begin
  inherited Create;
  FReportLink := AReportLink;
  FContainer := AContainer;
  FParentBuilder := AParentBuilder;
  FParentHost := AParentHost;
  ReportLink.FActiveBuilder := Self;
end;

destructor TdxPSContainerBuilder.Destroy;
begin
  ReportLink.FActiveBuilder := ParentBuilder;
  inherited;
end;

function TdxPSContainerBuilder.Build: TdxReportCell;
var
  Item: TdxReportVisualItem;
begin
  Result := CreateHost;
  BuildNestedControls;
  Item := BuildControl(Container);
  Item.Index := 0; // make it as first item
end;

function TdxPSContainerBuilder.BuildControl(AControl: TControl; AParentHost: TdxReportCell = nil): TdxReportVisualItem;
begin
  FCurrentControl := AControl;
  try
    with Producers[AControl] do
    begin
      if AParentHost = nil then AParentHost := Host;
      Result := ItemClass.Create(AParentHost);
      CreateItemDefinition(AControl, Result);
      InitializeItem(Result);
    end;
    InitializeItem(Result);
  finally
    FCurrentControl := nil;
  end;
end;

function TdxPSContainerBuilder.BuildNestedContainer(AContainer: TWinControl;
  AParentHost: TdxReportCell = nil): TdxReportCell;
begin
  Result := ReportLink.BuildContainer(AContainer, Self, AParentHost);
end;

class function TdxPSContainerBuilder.ContainerClass: TWinControlClass;
begin
  Result := TWinControl;
end;

class function TdxPSContainerBuilder.PairClass: TClass;
begin
  Result := ContainerClass;
end;

class procedure TdxPSContainerBuilder.Register;
begin
  dxContainerBuilderFactory.Register(Self);
end;

class procedure TdxPSContainerBuilder.Unregister;
begin
  if not FIsContainerBuilderFactoryReleased then dxContainerBuilderFactory.Unregister(Self);
end;

procedure TdxPSContainerBuilder.BuildNestedControls;
var
  Control: TControl;
  Count, Index: Integer;
begin
  if HasAvailableChildren(Container) then
    with CreateNestedControlsIterator do
    begin
      Count := ControlCount;
      Index := 1;

      // Controls
      GoBeyondEOF;
      while not IsBOF do
      begin
        Control := Prev;
        if not (Control is TWinControl) and ReportLink.DoIsComponentProcessed(Control) then
          BuildControl(Control);
        if IsRoot then
        begin
          Progress(100 * Index / Count);
          if IsAborted then Break;
        end;
        Inc(Index);
      end;
      if IsAborted then Exit;

      // WinControls
      GoBeforeBOF;
      while not IsEOF do
      begin
        Control := Next;
        if (Control is TWinControl) and ReportLink.DoIsComponentProcessed(Control) then
          BuildNestedContainer(TWinControl(Control), Host);
        if IsRoot then
        begin
          Progress(100 * Index / Count);
          if IsAborted then Break;
        end;
        Inc(Index);
      end;

      if not IsAborted then Progress(100);
    end;
end;

function TdxPSContainerBuilder.CreateHost: TdxReportCell;
begin
  FHost := Producers[Container].HostClass.Create(ParentHost);
  InitializeHost;
  Result := Host;
end;

function TdxPSContainerBuilder.CreateItemDefinition(AComponent: TComponent;
  AnItem: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;
begin
  Result := ReportLink.CreateItemDefinition(AComponent, AnItem);
end;

function TdxPSContainerBuilder.CreateNestedControlsIterator: IdxPSContainerIterator;
begin
  Result := Producers[Container].CreateIterator;
end;

procedure TdxPSContainerBuilder.InitializeHost;
begin
  Producers[Container].InitializeHost(Host);
end;

procedure TdxPSContainerBuilder.InitializeItem(AnItem: TdxReportVisualItem);
begin
  ReportLink.DoInitializeItem(AnItem);
  ReportLink.DoInitializeItemOptionsPlace(AnItem);
end;

function TdxPSContainerBuilder.IsAborted: Boolean;
begin
  Result := ReportLink.AbortBuilding;
end;

procedure TdxPSContainerBuilder.Progress(const APercentDone: Double);
begin
  ReportLink.DoProgress(APercentDone);
end;

function TdxPSContainerBuilder.GetParentHost: TdxReportCell;
begin
  Result := FParentHost;
  if Result = nil then
    if ParentBuilder <> nil then
      Result := ParentBuilder.Host
    else
      Result := ReportLink.RootCell;
end;

function TdxPSContainerBuilder.HasAvailableChildren(AControl: TControl): Boolean;
var
  Producer: TdxPSCustomContainerItemProducer;
  I: Integer;
  Child: TControl;
begin
  Result := False;
  if (AControl is TWinControl) and (TWinControl(AControl).ControlCount <> 0) then
  begin
    Producer := Producers[AControl];
    if Producer.CanHasAvailableChildren then
      with TWinControl(AControl) do
        for I := 0 to ControlCount - 1 do
        begin
          Child := Controls[I];
          Result := Producer.CanProcessChild(Child) and ReportLink.DoIsComponentProcessed(Child);
          if Result then Exit;
        end;
  end;
end;

function TdxPSContainerBuilder.GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

function TdxPSContainerBuilder.GetIsRoot: Boolean;
begin
  Result := ParentBuilder = nil;
end;

function TdxPSContainerBuilder.GetProducer(AControl: TControl): TdxPSCustomContainerItemProducer;
begin
  Result := ReportLink.Producers[AControl] as TdxPSCustomContainerItemProducer;
end;

{ TdxPSContainerReportLinkCustomCache }

constructor TdxPSContainerReportLinkCustomCache.Create(AReportLink: TdxCustomContainerReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
end;

function TdxPSContainerReportLinkCustomCache.ReportLink: TdxCustomContainerReportLink;
begin
  Result := FReportLink;
end;

{ TdxPSCustomProducerCache }

function TdxPSCustomProducerCache.GetItem(Index: Integer): TdxPSCustomProducer;
begin
   Result := TdxPSCustomProducer(inherited Items[Index]);
end;

function TdxPSCustomProducerCache.GetProducer(ProducerClass: TdxPSCustomProducerClass;
  Component: TComponent): TdxPSCustomProducer;
var
  Index: Integer;
begin
  Index := IndexOfByClass(ProducerClass);
  if (Index = -1) or (not ProducerClass.Reenterable and (Items[Index].ProducingObject <> Component)) then
    Index := Add(ProducerClass.Create(ReportLink, Component));
  Result := Items[Index];
  Result.Initialize(Component);
end;

{ TdxCustomContainerReportLinkOptions }

constructor TdxCustomContainerReportLinkOptions.Create(AReportLink: TdxCustomContainerReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  RestoreDefaults;
end;

procedure TdxCustomContainerReportLinkOptions.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptions then
  else
    inherited;
end;

procedure TdxCustomContainerReportLinkOptions.RestoreDefaults;
begin
end;

procedure TdxCustomContainerReportLinkOptions.Changed;
begin
  if ReportLink <> nil then
    ReportLink.OptionsModified(Self);
end;

{ TdxCustomContainerOptionsBehavior }

procedure TdxCustomContainerReportLinkOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsBehavior then
    with TdxCustomContainerReportLinkOptionsBehavior(Source) do
    begin
      Self.ConsumeExistingLinks := ConsumeExistingLinks;
      Self.LabelAutoHeight := LabelAutoHeight;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsBehavior.RestoreDefaults;
begin
  ConsumeExistingLinks := True;
  LabelAutoHeight := True;
end;

procedure TdxCustomContainerReportLinkOptionsBehavior.SetConsumeExistingLinks(Value: Boolean);
begin
  if FConsumeExistingLinks <> Value then
  begin
    FConsumeExistingLinks := Value;
    Changed;
  end;
end;

procedure TdxCustomContainerReportLinkOptionsBehavior.SetLabelAutoHeight(AValue: Boolean);
begin
  if AValue <> FLabelAutoHeight then
  begin
    FLabelAutoHeight := AValue;
    Changed;
  end;
end;

{ TdxCustomContainerReportLinkOptionsDesignerTabs }

procedure TdxCustomContainerReportLinkOptionsDesignerTabs.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsDesignerTabs then
    with TdxCustomContainerReportLinkOptionsDesignerTabs(Source) do
    begin
      Self.AutoHideReportLinksIfEmpty := AutoHideReportLinksIfEmpty;
      Self.Controls := Controls;
      Self.ReportLinks := ReportLinks;
      Self.Behaviors := Behaviors;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsDesignerTabs.RestoreDefaults;
begin
  inherited;
  AutoHideReportLinksIfEmpty := True;
  Controls := True;
  ReportLinks := True;
  Behaviors := False;
end;

{ TdxCustomContainerReportLinkOptionsItemPlace }

procedure TdxCustomContainerReportLinkOptionsItemPlace.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsItemPlace then
    with TdxCustomContainerReportLinkOptionsItemPlace(Source) do
    begin
      Self.ExpandHeight := ExpandHeight;
      Self.ExpandWidth := ExpandWidth;
      Self.ShrinkHeight := ShrinkHeight;
      Self.ShrinkWidth := ShrinkWidth;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.RestoreDefaults;
begin
  ExpandHeight := False;
  ExpandWidth := False;
  ShrinkHeight := False;
  ShrinkWidth := False;
end;

function TdxCustomContainerReportLinkOptionsItemPlace.GetData: Integer;
begin
  Result := ((Ord(ExpandHeight) shl ExpandHeightOffset) and ExpandHeightBit) or
            ((Ord(ExpandWidth) shl ExpandWidthOffset) and ExpandWidthBit) or
            ((Ord(ShrinkHeight) shl ShrinkHeightOffset) and ShrinkHeightBit) or
            ((Ord(ShrinkWidth) shr ShrinkWidthOffset) and ShrinkWidthBit);
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.SetData(Value: Integer);
begin
  ExpandHeight := Boolean((Value and ExpandHeightOffset) shr ExpandHeightBit);
  ExpandWidth := Boolean((Value and ExpandWidthOffset) shr ExpandWidthBit);
  ShrinkHeight := Boolean((Value and ShrinkHeightOffset) shr ShrinkHeightBit);
  ShrinkWidth := Boolean((Value and ShrinkWidthOffset) shr ShrinkWidthBit);
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.ReadData(AStream: TStream);
var
  Buffer: Integer;
begin
  Buffer := GetData;
  AStream.WriteBuffer(Buffer , SizeOf(Buffer));
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.WriteData(AStream: TStream);
var
  Buffer: Integer;
begin
  AStream.ReadBuffer(Buffer , SizeOf(Buffer));
  SetData(Buffer);
end;

function TdxCustomContainerReportLinkOptionsItemPlace.HasHorzResizing: Boolean;
begin
  Result := ExpandWidth or ShrinkWidth;
end;

function TdxCustomContainerReportLinkOptionsItemPlace.HasVertResizing: Boolean;
begin
  Result := ExpandHeight or ShrinkHeight;
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.SetAll;
begin
  ExpandHeight := True;
  ExpandWidth := True;
  ShrinkHeight := True;
  ShrinkWidth := True;
end;

procedure TdxCustomContainerReportLinkOptionsItemPlace.UnsetAll;
begin
  ExpandHeight := False;
  ExpandWidth := False;
  ShrinkHeight := False;
  ShrinkWidth := False;
end;

{ TdxCustomContainerReportLinkOptionsPagination }

procedure TdxCustomContainerReportLinkOptionsPagination.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsPagination then
    with TdxCustomContainerReportLinkOptionsPagination(Source) do
    begin
      Self.ControlDetails := ControlDetails;
      Self.Controls := Controls;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsPagination.RestoreDefaults;
begin
  inherited;
  ControlDetails := True;
  Controls := True;
end;

procedure TdxCustomContainerReportLinkOptionsPagination.SetControlDetails(Value: Boolean);
begin
  if FControlDetails <> Value then
  begin
    FControlDetails := Value;
    Changed;
  end;
end;

procedure TdxCustomContainerReportLinkOptionsPagination.SetControls(Value: Boolean);
begin
  if FControls <> Value then
  begin
    FControls := Value;
    Changed;
  end;
end;

{ TdxCustomContainerReportLinkOptionsRefinements }

procedure TdxCustomContainerReportLinkOptionsRefinements.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsRefinements then
    with TdxCustomContainerReportLinkOptionsRefinements(Source) do
    begin
      Self.RootBorders := RootBorders;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsRefinements.RestoreDefaults;
begin
  inherited;
  RootBorders := False;
end;

procedure TdxCustomContainerReportLinkOptionsRefinements.SetRootBorders(Value: Boolean);
begin
  if FRootBorders <> Value then
  begin
    FRootBorders := Value;
    Changed;
  end;
end;

{ TdxCustomContainerOptionsTransparent }

procedure TdxCustomContainerReportLinkOptionsTransparent.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLinkOptionsTransparent then
    with TdxCustomContainerReportLinkOptionsTransparent(Source) do
    begin
      Self.Containers := Containers;
      Self.Controls := Controls;
      Self.Graphics := Graphics;
      Self.Root := Root;
    end;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsTransparent.RestoreDefaults;
begin
  Controls := True;
  Containers := True;
  Graphics := False;
  Root := True;
  inherited;
end;

procedure TdxCustomContainerReportLinkOptionsTransparent.SetContainters(Value: Boolean);
begin
  if FContainers <> Value then
  begin
    FContainers := Value;
    Changed;
  end;
end;

procedure TdxCustomContainerReportLinkOptionsTransparent.SetControls(Value: Boolean);
begin
  if FControls <> Value then
  begin
    FControls := Value;
    Changed;
  end;
end;

procedure TdxCustomContainerReportLinkOptionsTransparent.SetGraphics(Value: Boolean);
begin
  if FGraphics <> Value then
  begin
    FGraphics := Value;
    Changed;
  end;
end;

procedure TdxCustomContainerReportLinkOptionsTransparent.SetRoot(Value: Boolean);
begin
  if FRoot <> Value then
  begin
    FRoot := Value;
    Changed;
  end;
end;

{ TdxCustomContainerReportLink }

constructor TdxCustomContainerReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FAggregatedReportLinks := TStringList.Create;
  FDefinitions := TList.Create;
  FDelimitersHorz := TList.Create;
  FDelimitersVert := TList.Create;
  FExcludedComponents := TStringList.Create;
  FHiddenComponents := TStringList.Create;
  FPreparationFont := TFont.Create;
  FProducerCache := TdxPSCustomProducerCache.Create(Self);
  FSupportedCustomDraw := False;
  CreateOptions;
end;

destructor TdxCustomContainerReportLink.Destroy;
begin
  DestroyOptions;
  FreeAndNil(FPreparationFont);
  FreeAndNil(FProducerCache);
  FreeAndNil(FHiddenComponents);
  FreeAndNil(FExcludedComponents);
  FreeAndNil(FDelimitersVert);
  FreeAndNil(FDelimitersHorz);
  FreeAndNilDefinitions;
  FreeAndNil(FAggregatedReportLinks);
  inherited;
end;

procedure TdxCustomContainerReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomContainerReportLink then
    with TdxCustomContainerReportLink(Source) do
    begin
      Self.FAggregatedReportLinks.Assign(FAggregatedReportLinks);
      Self.FExcludedComponents.Assign(FExcludedComponents);
      Self.FHiddenComponents.Assign(FHiddenComponents);
      Self.OptionsBehavior := OptionsBehavior;
      Self.OptionsDesignerTabs := OptionsDesignerTabs;
      //Self.OptionsItemPlace := OptionsItemPlace; {.2}
      Self.OptionsPagination := OptionsPagination;
      Self.OptionsTransparent := OptionsTransparent;
      Self.SupportedCustomDraw := SupportedCustomDraw;
    end;
  inherited;
end;

class function TdxCustomContainerReportLink.Aggregable: Boolean;
begin
  Result := False;
end;

function TdxCustomContainerReportLink.CanHideComponent(AComponent: TComponent): Boolean;
begin
  Result := (AComponent <> nil) and (Container <> nil) and (AComponent <> Container);
  if Result then
    if AComponent is TControl then
      Result := Container.ContainsControl(TControl(AComponent))
    else
      Result := (Container.Owner <> nil) and (Container.Owner = AComponent.Owner);
end;

function TdxCustomContainerReportLink.CanHideComponentByName(const AName: string): Boolean;
begin
  Result := (AName <> '') and (Container <> nil);
  if Result then
  begin
    Result := Container.FindChildControl(AName) <> nil;
    if not Result and (Container.Owner <> nil) then
      Result := Container.Owner.FindComponent(AName) <> nil;
  end;
end;

procedure TdxCustomContainerReportLink.HideComponent(AComponent: TComponent);
var
  Index: Integer;
begin
  if CanHideComponent(AComponent) and not FindHiddenComponent(AComponent, Index) then
    FHiddenComponents.AddObject(AComponent.Name, AComponent)
end;

procedure TdxCustomContainerReportLink.HideComponentByName(const AName: string);
var
  Index: Integer;
  Component: TComponent;
begin
  if CanHideComponentByName(AName) and not FindHiddenComponentByName(AName, Index) then
  begin
    Component := GetComponentByName(AName);
    if Component <> nil then
      FHiddenComponents.AddObject(AName, Component);
  end;
end;

procedure TdxCustomContainerReportLink.HideComponents(const AComponents: array of TComponent);
var
  I: Integer;
begin
  for I := Low(AComponents) to High(AComponents) do
    HideComponent(AComponents[I]);
end;

procedure TdxCustomContainerReportLink.HideComponentsByName(const ANames: array of string);
var
  I: Integer;
begin
  for I := Low(ANames) to High(ANames) do
    HideComponentByName(ANames[I]);
end;

function TdxCustomContainerReportLink.FindHiddenComponent(AComponent: TComponent;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FHiddenComponents.IndexOfObject(AComponent);
  Result := AnIndex <> -1;
end;

function TdxCustomContainerReportLink.FindHiddenComponent(AComponent: TComponent): Boolean;
var
  Index: Integer;
begin
  Result := FindHiddenComponent(AComponent, Index);
end;

function TdxCustomContainerReportLink.FindHiddenComponentByName(const AName: string;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FHiddenComponents.IndexOf(AName);
  Result := AnIndex <> -1;
end;

function TdxCustomContainerReportLink.FindHiddenComponentByName(const AName: string): Boolean;
var
  Index: Integer;
begin
  Result := FindHiddenComponentByName(AName, Index);
end;

procedure TdxCustomContainerReportLink.UnhideAllComponents;
begin
  FHiddenComponents.Clear;
end;

procedure TdxCustomContainerReportLink.UnhideComponent(AComponent: TComponent);
var
  Index: Integer;
begin
  if FindHiddenComponent(AComponent, Index) then
    FHiddenComponents.Delete(Index);
end;

procedure TdxCustomContainerReportLink.UnhideComponentByName(const AName: string);
var
  Index: Integer;
begin
  if FindHiddenComponentByName(AName, Index) then
    FHiddenComponents.Delete(Index);
end;

procedure TdxCustomContainerReportLink.UnhideComponents(const AComponents: array of TComponent);
var
  I: Integer;
begin
  for I := Low(AComponents) to High(AComponents) do
    UnhideComponent(AComponents[I]);
end;

procedure TdxCustomContainerReportLink.UnhideComponentsByName(const ANames: array of string);
var
  I: Integer;
begin
  for I := Low(ANames) to High(ANames) do
    UnhideComponentByName(ANames[I]);
end;

procedure TdxCustomContainerReportLink.HideStandardControls;
var
  I, J: Integer;
  ControlClass: TClass;
  Control: TControl;
begin
  if Container <> nil then
    for I := Low(StandardHiddenControls) to High(StandardHiddenControls) do
    begin
      ControlClass := Classes.GetClass(StandardHiddenControls[I]);
      if ControlClass <> nil then
        for J := 0 to Container.ControlCount - 1 do
        begin
          Control := Container.Controls[J];
          if Control is ControlClass then
            HideComponent(Control);
        end;
    end;
end;

procedure TdxCustomContainerReportLink.UnhideStandardControls;
var
  I, J: Integer;
  ControlClass: TClass;
  Control: TControl;
begin
  if Container <> nil then
    for I := Low(StandardHiddenControls) to High(StandardHiddenControls) do
    begin
      ControlClass := Classes.GetClass(StandardHiddenControls[I]);
      if ControlClass <> nil then
        for J := 0 to Container.ControlCount - 1 do
        begin
          Control := Container.Controls[J];
          if Control is ControlClass then
            UnhideComponent(Control);
        end;
    end;
end;

function TdxCustomContainerReportLink.CanExcludeComponent(AComponent: TComponent): Boolean;
begin
  Result := (AComponent <> nil) and (Container <> nil) and (AComponent <> Container);
  if Result then
    if AComponent is TControl then
      Result := Container.ContainsControl(TControl(AComponent))
    else
      Result := (Container.Owner <> nil) and (Container.Owner = AComponent.Owner);
end;

function TdxCustomContainerReportLink.CanExcludeComponentByName(const AName: string): Boolean;
begin
  Result := (AName <> '') and (Container <> nil);
  if Result then
  begin
    Result := Container.FindChildControl(AName) <> nil;
    if not Result and (Container.Owner <> nil) then
      Result := Container.Owner.FindComponent(AName) <> nil;
  end;
end;

procedure TdxCustomContainerReportLink.ExcludeComponent(AComponent: TComponent);
var
  Index: Integer;
begin
  if CanExcludeComponent(AComponent) and not FindExcludedComponent(AComponent, Index) then
    FExcludedComponents.AddObject(AComponent.Name, AComponent);
end;

procedure TdxCustomContainerReportLink.ExcludeComponentByName(const AName: string);
var
  Index: Integer;
  Component: TComponent;
begin
  if CanExcludeComponentByName(AName) and not FindExcludedComponentByName(AName, Index) then
  begin
    Component := GetComponentByName(AName);
    if Component <> nil then
      FExcludedComponents.AddObject(AName, Component);
  end;
end;

procedure TdxCustomContainerReportLink.ExcludeComponents(const AComponents: array of TComponent);
var
  I: Integer;
begin
  for I := Low(AComponents) to High(AComponents) do
    ExcludeComponent(AComponents[I]);
end;

procedure TdxCustomContainerReportLink.ExcludeComponentsByName(const ANames: array of string);
var
  I: Integer;
begin
  for I := Low(ANames) to High(ANames) do
    ExcludeComponentByName(ANames[I]);
end;

function TdxCustomContainerReportLink.FindExcludedComponent(AComponent: TComponent;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FExcludedComponents.IndexOfObject(AComponent);
  Result := AnIndex <> -1;
end;

function TdxCustomContainerReportLink.FindExcludedComponent(AComponent: TComponent): Boolean;
var
  Index: Integer;
begin
  Result := FindExcludedComponent(AComponent, Index);
end;

function TdxCustomContainerReportLink.FindExcludedComponentByName(const AName: string;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FExcludedComponents.IndexOf(AName);
  Result := AnIndex <> -1;
end;

function TdxCustomContainerReportLink.FindExcludedComponentByName(const AName: string): Boolean;
var
  Index: Integer;
begin
  Result := FindExcludedComponentByName(AName, Index);
end;

procedure TdxCustomContainerReportLink.UnexcludeAllComponents;
begin
  FExcludedComponents.Clear;
end;

procedure TdxCustomContainerReportLink.UnexcludeComponent(AComponent: TComponent);
var
  Index: Integer;
begin
  if FindExcludedComponent(AComponent, Index) then
    FExcludedComponents.Delete(Index);
end;

procedure TdxCustomContainerReportLink.UnexcludeComponentByName(const AName: string);
var
  Index: Integer;
begin
  if FindExcludedComponentByName(AName, Index) then
    FExcludedComponents.Delete(Index);
end;

procedure TdxCustomContainerReportLink.UnexcludeComponents(const AComponents: array of TComponent);
var
  I: Integer;
begin
  for I := Low(AComponents) to High(AComponents) do
    UnexcludeComponent(AComponents[I]);
end;

procedure TdxCustomContainerReportLink.UnexcludeComponentsByName(const ANames: array of string);
var
  I: Integer;
begin
  for I := Low(ANames) to High(ANames) do
    UnexcludeComponentByName(ANames[I]);
end;

procedure TdxCustomContainerReportLink.AggregateLink(AReportLink: TBasedxReportLink);
begin
  if IsLinkAggregable(AReportLink) and not IsLinkAggregated(AReportLink) then
    FAggregatedReportLinks.AddObject(AReportLink.Name, AReportLink);
end;

procedure TdxCustomContainerReportLink.DisaggregateAllLinks;
begin
  InstallAggregatedReportLinksController(False);
  FAggregatedReportLinks.Clear;
end;

procedure TdxCustomContainerReportLink.DisaggregateInconsistentLinks;
var
  I: Integer;
  Link: TBasedxReportLink;
begin
  for I := AggregatedReportLinkCount - 1 downto 0 do
  begin
    Link := AggregatedReportLinks[I];
    if not IsLinkAggregatedConsistently(Link) then DisaggregateLink(Link);
  end;
end;

procedure TdxCustomContainerReportLink.DisaggregateLink(AReportLink: TBasedxReportLink);
var
  Index: Integer;
begin
  Index := FAggregatedReportLinks.IndexOfObject(AReportLink);
  if Index <> -1 then
  begin
    TBasedxReportLinkAccess(AReportLink).Controller := nil;
    FAggregatedReportLinks.Delete(Index);
  end;
end;

function TdxCustomContainerReportLink.FindAggregatedLinkByComponent(AComponent: TComponent): TBasedxReportLink;
var
  I: Integer;
begin
  if AComponent <> nil then
    for I := 0 to AggregatedReportLinkCount - 1 do
    begin
      Result := AggregatedReportLinks[I];
      if (Result <> nil) and (Result.Component = AComponent) then
        Exit;
    end;
  Result := nil;
end;

function TdxCustomContainerReportLink.HasInconsistentlyAggregatedLinks: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to AggregatedReportLinkCount - 1 do
    if not IsLinkAggregatedConsistently(AggregatedReportLinks[I]) then
      Exit;
  Result := False;
end;

function TdxCustomContainerReportLink.IsLinkAggregable(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := (AReportLink <> nil) and (AReportLink <> Self) and AReportLink.Aggregable;
end;

function TdxCustomContainerReportLink.IsLinkAggregated(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := FAggregatedReportLinks.IndexOfObject(AReportLink) <> -1;
end;

function TdxCustomContainerReportLink.IsLinkAggregatedConsistently(AReportLink: TBasedxReportLink): Boolean;
begin
  Result := IsLinkAggregated(AReportLink) and (AReportLink.Component is TControl) and
    (Container <> nil) and Container.ContainsControl(TControl(AReportLink.Component));
end;

procedure TdxCustomContainerReportLink.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('HiddenComponents', ReadHiddenComponents,
    WriteHiddenComponents, True);
  Filer.DefineBinaryProperty('ExcludedComponents', ReadExcludedComponents,
    WriteExcludedComponents, True);
  Filer.DefineBinaryProperty('AggregatedReportLinks', ReadAggregatedReportLinks,
    WriteAggregatedReportLinks, True);
end;

procedure TdxCustomContainerReportLink.Loaded;
begin
  inherited;
  LoadHiddenComponents;
  LoadExcludedComponents;
  LoadAggregatedReportLinks;
end;

procedure TdxCustomContainerReportLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and not IsDestroying then
  begin
    UnexcludeComponent(AComponent);
    UnhideComponent(AComponent);
    if AComponent is TBasedxReportLink then
      DisaggregateLink(TBasedxReportLink(AComponent));
  end;
end;

procedure TdxCustomContainerReportLink.AfterDesignReport(ADone: Boolean);
begin
  InstallAggregatedReportLinksController(False);
  inherited;
end;

procedure TdxCustomContainerReportLink.BeforeDesignReport;
begin
  inherited;
  InstallAggregatedReportLinksController(True);
  HideDesignerTabs(DesignWindow);
end;

procedure TdxCustomContainerReportLink.ConstructReport(AReportCells: TdxReportCells);
var
  Root: TdxReportCell;
begin
  inherited;
  if (Container = nil) or (csLoading in Container.ComponentState) then Exit;

  PrepareConstruct;
  try
    Root := BuildContainer(Container, nil, nil);
    if not AbortBuilding then
    begin
      RepositionControls;
      Root.Left := 0;
      Root.Top := 0;
      AReportCells.Cells.BoundsRect := Rect(0, 0, Root.Width, Root.Height);
    end;
  finally
    UnprepareConstruct;
  end;
end;

procedure TdxCustomContainerReportLink.ConvertCoords;

  procedure ConvertDelimiters(ADelimiters: TList);
  var
    I, Value: Integer;
  begin
    for I := 0 to ADelimiters.Count - 1 do
    begin
      Value := Integer(ADelimiters[I]);
      Value := MulDiv(Value, PixelsNumerator, PixelsDenominator);
      ADelimiters[I] := Pointer(Value);
    end;
  end;

begin
  inherited;
  ConvertDelimiters(DelimitersHorz);
  ConvertDelimiters(DelimitersVert);
end;

procedure TdxCustomContainerReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
begin
  if AItem.Data <> 0 then DoCustomDrawItem(ACanvas, AItem, ADone);
end;

procedure TdxCustomContainerReportLink.DoApplyInDesigner;
begin
  inherited;
  InstallAggregatedReportLinksController(True);
end;

procedure TdxCustomContainerReportLink.DoChangeComponent;
begin
  inherited;
  DisaggregateAllLinks;
  UnexcludeAllComponents;
  UnhideAllComponents;
end;

function TdxCustomContainerReportLink.GetDesignerClass: TdxReportLinkDesignWindowClass;
begin
  Result := inherited GetDesignerClass;
  if (Result = nil) and ((Component is TForm) or (Component is TFrame)) then
    Result := TdxfmCustomContainerDesignWindow;
end;

procedure TdxCustomContainerReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
var
  I: Integer;
begin
  inherited;
  for I := 0 to DefinitionCount - 1 do
    Producers[Definitions[I].Component].GetImageLists(AProc);
end;

procedure TdxCustomContainerReportLink.InternalRestoreDefaults;
begin
  inherited;

  OptionsBehavior.RestoreDefaults;
  OptionsDesignerTabs.RestoreDefaults;
  //OptionsItemPlace.RestoreDefaults; {.2}
  OptionsPagination.RestoreDefaults;
  OptionsRefinements.RestoreDefaults;
  OptionsTransparent.RestoreDefaults;

  SupportedCustomDraw := False;
end;

function TdxCustomContainerReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := SupportedCustomDraw and (Item <> nil) and (Item.Data <> 0);
end;

procedure TdxCustomContainerReportLink.MakeDelimiters(AReportCells: TdxReportCells;
  AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited;
  dxAppendList(DelimitersHorz, AHorzDelimiters);
  dxAppendList(DelimitersVert, AVertDelimiters);
end;

function TdxCustomContainerReportLink.BuildContainer(AContainer: TWinControl;
  AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil): TdxReportCell;
begin
  with CreateBuilder(AContainer, AParentBuilder, AParentHost) do
  try
    Result := Build;
  finally
    Free;
  end;
end;

function TdxCustomContainerReportLink.CreateBuilder(AContainer: TWinControl;
  AParentBuilder: TdxPSContainerBuilder; AParentHost: TdxReportCell = nil): TdxPSContainerBuilder;
begin
  Result := dxContainerBuilderFactory.CreateBuilder(Self, AContainer, AParentBuilder, AParentHost);
end;

function TdxCustomContainerReportLink.CreateItemDefinition(AComponent: TComponent;
  AnItem: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;
begin
  with Producers[AComponent] do
  begin
    Result := DefinitionClass.Create(Self, AComponent, AnItem);
    InitializeOptionsPlace(Result.OptionsPlace);
  end;
  AddDefinition(Result);
end;

function TdxCustomContainerReportLink.GetProducer(Component: TComponent): TdxPSCustomProducer;
begin
  Result := ProducerCache[dxPSContainerItemProducerFactory[Component, Component = Container], Component];
end;

// IdxReportLinkController
function TdxCustomContainerReportLink.GetControlSiteBounds(AControl: TControl): TRect;
begin
  Result := AControl.BoundsRect;
end;

procedure TdxCustomContainerReportLink.DoCustomDrawItem(
  ACanvas: TCanvas; AnItem: TAbstractdxReportCellData; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawItem) then
    FOnCustomDrawItem(Self, ACanvas, AnItem, TComponent(TObject(AnItem.Data)), ADone);
end;

procedure TdxCustomContainerReportLink.DoGetComponentCaption(AComponent: TComponent;
  var ACaption: string);
begin
  if Assigned(FOnGetComponentCaption) then
    FOnGetComponentCaption(Self, AComponent, ACaption)
end;

function TdxCustomContainerReportLink.DoGetReportLink(AComponent: TComponent): TBasedxReportLink;
begin
  Result := nil;
  //if OptionsBehavior.ConsumeExistingLinks then
    Result := FindAggregatedLinkByComponent(AComponent);
  if Assigned(FOnGetComponentReportLink) then
    FOnGetComponentReportLink(Self, AComponent, Result);
  if Result <> nil then
    Result.Initialize;
end;

procedure TdxCustomContainerReportLink.DoInitializeItem(AnItem: TdxReportVisualItem);
begin
  if Assigned(FOnInitializeItem) then
    FOnInitializeItem(Self, AnItem, TComponent(TObject(AnItem.Data)));
end;

procedure TdxCustomContainerReportLink.DoInitializeItemOptionsPlace(AnItem: TdxReportVisualItem);
begin
  if Assigned(FOnInitializeItemOptionsPlace) then
    FOnInitializeItemOptionsPlace(Self, AnItem, TComponent(TObject(AnItem.Data)),
    DefinitionsByReportItem[AnItem].OptionsPlace);
end;

procedure TdxCustomContainerReportLink.DoInitializeReportLink(AReportLink: TBasedxReportLink;
  AStage: TdxPSReportLinkProcessingStage);
begin
  if Assigned(FOnInitializeReportLink) then
    FOnInitializeReportLink(Self, AReportLink, AStage);
end;

function TdxCustomContainerReportLink.DoIsComponentProcessed(AComponent: TComponent): Boolean;
var
  Index: Integer;
begin
  Result := (not (AComponent is TControl) or
    (TControl(AComponent).Visible and not IsRectEmpty(TControl(AComponent).BoundsRect))) and
    not FindHiddenComponent(AComponent, Index) and not FindExcludedComponent(AComponent, Index);
  if Assigned(FOnIsComponentProcessed) then
    FOnIsComponentProcessed(Self, AComponent, Result);
end;

procedure TdxCustomContainerReportLink.CreateOptions;
begin
  FOptionsBehavior := GetOptionsBehaviorClass.Create(Self);
  FOptionsDesignerTabs := GetOptionsDesignerTabsClass.Create(Self);
  //FOptionsItemPlace := GetOptionsItemPlaceClass.Create(Self); {.2}
  FOptionsPagination := GetOptionsPaginationClass.Create(Self);
  FOptionsRefinements := GetOptionsRefinementsClass.Create(Self);
  FOptionsTransparent := GetOptionsTransparentClass.Create(Self);
end;

procedure TdxCustomContainerReportLink.DestroyOptions;
begin
  FreeAndNil(FOptionsTransparent);
  FreeAndNil(FOptionsRefinements);
  FreeAndNil(FOptionsPagination);
  //FreeAndNil(FOptionsItemPlace); {.2}
  FreeAndNil(FOptionsDesignerTabs);
  FreeAndNil(FOptionsBehavior);
end;

function TdxCustomContainerReportLink.GetOptionsBehaviorClass: TdxCustomContainerReportLinkOptionsBehaviorClass;
begin
  Result := TdxCustomContainerReportLinkOptionsBehavior;
end;

function TdxCustomContainerReportLink.GetOptionsDesignerTabsClass: TdxCustomContainerReportLinkOptionsDesignerTabsClass;
begin
  Result := TdxCustomContainerReportLinkOptionsDesignerTabs;
end;

function TdxCustomContainerReportLink.GetOptionsItemPlaceClass: TdxCustomContainerReportLinkOptionsItemPlaceClass;
begin
  Result := TdxCustomContainerReportLinkOptionsItemPlace;
end;

function TdxCustomContainerReportLink.GetOptionsPaginationClass: TdxCustomContainerReportLinkOptionsPaginationClass;
begin
  Result := TdxCustomContainerReportLinkOptionsPagination;
end;

function TdxCustomContainerReportLink.GetOptionsRefinementsClass: TdxCustomContainerReportLinkOptionsRefinementsClass;
begin
  Result := TdxCustomContainerReportLinkOptionsRefinements;
end;

function TdxCustomContainerReportLink.GetOptionsTransparentClass: TdxCustomContainerReportLinkOptionsTransparentClass;
begin
  Result := TdxCustomContainerReportLinkOptionsTransparent;
end;

procedure TdxCustomContainerReportLink.OptionsModified(AnOptions: TdxCustomContainerReportLinkOptions);
begin
  LinkModified(True);
end;

procedure TdxCustomContainerReportLink.PrepareConstruct;
begin
  ClearDefinitions;
  FScreenCanvas := TdxPSReportRenderScreenCanvas.Create;
  CreateRootLookAndFeel;
end;

procedure TdxCustomContainerReportLink.UnprepareConstruct;
begin
  if not AbortBuilding then
  begin
    PullReportItems;
    AddDelimiters;
  end;
  FreeAndNil(FScreenCanvas);
end;

procedure TdxCustomContainerReportLink.AddDefinition(ADefinition: TdxPSCustomContainerItemDefinition);
begin
  FDefinitions.Add(ADefinition);
end;

procedure TdxCustomContainerReportLink.ClearDefinitions;
var
  I: Integer;
begin
  for I := 0 to DefinitionCount - 1 do
    Definitions[I].Free;
  FDefinitions.Clear;
end;

function TdxCustomContainerReportLink.FindDefinition(AComponent: TComponent;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FDefinitions.IndexOf(DefinitionsByContainerItem[AComponent]);
  Result := AnIndex <> -1;
end;

function TdxCustomContainerReportLink.FindDefinition(AnItem: TdxReportVisualItem;
  out AnIndex: Integer): Boolean;
begin
  AnIndex := FDefinitions.IndexOf(DefinitionsByReportItem[AnItem]);
  Result := AnIndex <> -1;
end;

procedure TdxCustomContainerReportLink.FreeAndNilDefinitions;
begin
  ClearDefinitions;
  FreeAndNil(FDefinitions);
end;

procedure TdxCustomContainerReportLink.DeleteDefinition(Index: Integer);
begin
  Definitions[Index].Free;
  FDefinitions.Delete(Index);
end;

procedure TdxCustomContainerReportLink.AddControl(ATreeView: TTreeView;
  AParent: TTreeNode; AControl: TControl);
var
  Index, I: Integer;
  Control: TControl;
begin
  AParent := AddNode(ATreeView, AParent, AControl, not FindExcludedComponent(AControl, Index));
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      Control := TWinControl(AControl).Controls[I];
      if Producers[AControl].CanProcessChild(Control) and not FindHiddenComponent(Control, Index) then
        AddControl(ATreeView, AParent, Control);
    end;
end;

procedure TdxCustomContainerReportLink.AddHiddenControl(ATreeView: TTreeView;
  AParent: TTreeNode; AControl: TControl);
var
  Index, I: Integer;
  Control: TControl;
begin
  AParent := AddNode(ATreeView, AParent, AControl, not FindHiddenComponent(AControl, Index));
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      Control := TWinControl(AControl).Controls[I];
      if Producers[AControl].CanProcessChild(Control) then
        AddHiddenControl(ATreeView, AParent, Control);
    end;
end;

function TdxCustomContainerReportLink.AddNode(ATreeView: TTreeView;
  AParent: TTreeNode; AComponent: TComponent; AChecked: Boolean): TTreeNode;

  function PrepareText(const S: string): string;

    procedure DoTrancateString(var S: string);
    begin
      if Length(S) > 255 then
      begin
        Delete(S, 256, Length(S) - 255);
        Delete(S, Length(S) - 3, 3);
        S := S + '...';
      end;
    end;

  begin
    Result := S;
    DoTrancateString(Result);
    Result := RemoveAccelChars(Result, False);
  end;

var
  NodeObject: TdxNodeObject;
  S: string;
begin
  NodeObject := CreateNodeObject(AComponent, AChecked);
  S := PrepareText(NodeObject.Caption);
  Result := ATreeView.Items.AddChildObject(AParent, S, NodeObject);
  Result.StateIndex := dxPSUtl.dxCheckStateImageIndexMap(NodeObject.State);
end;

function TdxCustomContainerReportLink.CreateNodeObject(AComponent: TComponent;
  AChecked: Boolean): TdxNodeObject;
const
  StateMap: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
begin
  Result := TdxNodeObject.Create;
  with Result do
  begin
    Caption := Producers[AComponent].ProducingObjectFriendlyName;
    Component := AComponent;
    DoGetComponentCaption(AComponent, Caption);
    State := StateMap[AChecked];
  end;
end;

function TdxCustomContainerReportLink.IsComponentEditable(AComponent: TComponent): Boolean;
begin
  Result := AComponent <> Container;
end;

procedure TdxCustomContainerReportLink.LoadControlsTree(ATreeView: TTreeView);
begin
  ATreeView.Items.Clear;
  if Container <> nil then
    AddControl(ATreeView, nil, Container);
end;

procedure TdxCustomContainerReportLink.LoadHiddenControlsTree(ATreeView: TTreeView);
begin
  ATreeView.Items.Clear;
  if Container <> nil then
    AddHiddenControl(ATreeView, nil, Container);
end;

procedure TdxCustomContainerReportLink.InstallAggregatedReportLinksController(AnInstall: Boolean);
var
  I: Integer;
begin
  for I := 0 to AggregatedReportLinkCount - 1 do
    with TBasedxReportLinkAccess(AggregatedReportLinks[I]) do
      if AnInstall then
        Controller := Self
      else
        Controller := nil;
end;

function TdxCustomContainerReportLink.GetComponentByName(const AName: string): TComponent;
begin
  if AName <> '' then
  begin
    Result := Container.FindChildControl(AName);
    if (Result = nil) and (Container <> nil) and (Container.Owner <> nil) then
      Result := Container.Owner.FindComponent(AName);
  end
  else
    Result := nil;
end;

function TdxCustomContainerReportLink.GetPreparedFontIndex(AFont: TFont): Integer;
begin
  Result := GetPreparedFontIndex(AFont, True, AFont.Name, AFont.Color, AFont.Style, AFont.Orientation);
end;

function TdxCustomContainerReportLink.GetPreparedFontIndex(AFont: TFont; AIsFontSubstitutable: Boolean;
  const AFontName: string; AFontColor: TColor; AFontStyle: TFontStyles; AFontOrientation: Integer): Integer;
begin
  PreparationFont.Assign(AFont);
  if AIsFontSubstitutable and not dxIsTrueTypeFont(AFont) then
    PreparationFont.Name := Font.Name
  else
    PreparationFont.Name := AFontName;

  PreparationFont.Orientation := AFontOrientation;
  PreparationFont.Color := AFontColor;
  PreparationFont.Style := AFontStyle;
  Result := AddFontToPool(PreparationFont);
end;

function TdxCustomContainerReportLink.IsComponentProcessed(AComponent: TComponent): Boolean;
begin
  Result := DefinitionsByContainerItem[AComponent] <> nil;
end;

procedure TdxCustomContainerReportLink.AddDelimiters;
var
  I: Integer;
begin
  DelimitersHorz.Clear;
  DelimitersVert.Clear;
  for I := 0 to DefinitionCount - 1 do
    with Definitions[I] do
    begin
      AddDelimitersHorz(DelimitersHorz);
      AddDelimitersVert(DelimitersVert);
    end;
end;

procedure TdxCustomContainerReportLink.CreateRootLookAndFeel;
begin
  ReportCells.LookAndFeel := CreateGroupLookAndFeel(TdxPSReportGroupStandardLookAndFeel);
  with ReportCells.LookAndFeel do
  begin
    FontIndex := AddFontToPool(Control_GetFont(Container));
    CaptionFontIndex := ReportCells.LookAndFeel.FontIndex;
    Color := Control_GetColor(Container);
  end;
end;

procedure TdxCustomContainerReportLink.HideDesignerTabs(ADesignWindow: TdxfmCustomContainerDesignWindow);
begin
  if not IsDesigning then
    with OptionsDesignerTabs do
    begin
      ADesignWindow.tshReportLinks.Visible := ReportLinks and ((AggregatedReportLinkCount <> 0) or not AutoHideReportLinksIfEmpty);
      ADesignWindow.tshControls.Visible := Controls;
      ADesignWindow.tshBehaviors.Visible := Behaviors;
    end;
end;

procedure TdxCustomContainerReportLink.PullReportItems;

  procedure PullNestedItem(AnItem: TdxReportVisualItem);
  begin
    with AnItem do
    begin
      Left := Left + Parent.Left;
      Top := Top + Parent.Top;
      Parent := Parent.Parent;
    end;
  end;

  function NeedToBeEliminated(AnItem: TdxReportVisualItem): Boolean;
  const
    MaxHeight = High(Word) div 2;
  begin
    Result := AnItem.Height > MaxHeight;
  end;

  procedure EnumNestedItems(ACell: TdxReportCell; APull: Boolean);
  var
    I, Index: Integer;
    Item: TdxReportVisualItem;
  begin
    for I := ACell.CellCount - 1 downto 0 do
    begin
      Item := ACell.Cells[I];
      if APull then PullNestedItem(Item);

      if NeedToBeEliminated(Item) then
      begin
        EnumNestedItems(TdxReportCell(Item), True);
        if FindDefinition(Item, Index) then DeleteDefinition(Index);
        Item.Free;
      end;
    end;

    for I := ACell.DataItemCount - 1 downto 0 do
    begin
      Item := ACell.DataItems[I];
      if NeedToBeEliminated(Item) then
      begin
        if FindDefinition(Item, Index) then DeleteDefinition(Index);
        Item.Free;
      end
      else
        if APull then PullNestedItem(Item);
    end;
  end;

begin
  if IsWin9X then
    EnumNestedItems(RootCell, False);
end;

procedure TdxCustomContainerReportLink.RepositionControls;

  procedure EnumContainer(AContainer: TWinControl);
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to AContainer.ControlCount - 1 do
    begin
      Control := AContainer.Controls[I];
      if IsComponentProcessed(Control) then
      begin
        if Control is TWinControl then
          EnumContainer(TWinControl(Control));
        Producers[Control].Reposition;
      end;
    end;
  end;

begin
  EnumContainer(Container);
  Producers[Container].Reposition;
end;

function TdxCustomContainerReportLink.NeedTwoPassRendering: Boolean;
begin
  Result := True;
end;

function TdxCustomContainerReportLink.IsScaleGridLines: Boolean;
begin
  Result := True;
end;

function TdxCustomContainerReportLink.GetAggregatedReportLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(FAggregatedReportLinks.Objects[Index]);
end;

function TdxCustomContainerReportLink.GetAggregatedReportLinkCount: Integer;
begin
  Result := FAggregatedReportLinks.Count;
end;

function TdxCustomContainerReportLink.GetContainer: TWinControl;
begin
  Result := TWinControl(Component);
end;

function TdxCustomContainerReportLink.GetController: TdxCustomContainerReportLink;
begin
  Result := inherited Controller as TdxCustomContainerReportLink;
end;

function TdxCustomContainerReportLink.GetDefinition(Index: Integer): TdxPSCustomContainerItemDefinition;
begin
  Result := TdxPSCustomContainerItemDefinition(FDefinitions[Index]);
end;

function TdxCustomContainerReportLink.GetDefinitionByContainerItem(Component: TComponent): TdxPSCustomContainerItemDefinition;
var
  I: Integer;
begin
  for I := 0 to DefinitionCount - 1 do
  begin
    Result := Definitions[I];
    if Result.Component = Component then Exit;
  end;
  Result := nil;
end;

function TdxCustomContainerReportLink.GetDefinitionByReportItem(Item: TdxReportVisualItem): TdxPSCustomContainerItemDefinition;
var
  I: Integer;
begin
  for I := 0 to DefinitionCount - 1 do
  begin
    Result := Definitions[I];
    if Result.ReportItem = Item then Exit;
  end;
  Result := nil;
end;

function TdxCustomContainerReportLink.GetDefinitionCount: Integer;
begin
  Result := FDefinitions.Count;
end;

function TdxCustomContainerReportLink.GetDelimitersHorzCount: Integer;
begin
  Result := DelimitersHorz.Count;
end;

function TdxCustomContainerReportLink.GetDelimitersHorzItem(Index: Integer): Integer;
begin
  Result := Integer(DelimitersHorz[Index]);
end;

function TdxCustomContainerReportLink.GetDelimitersVertCount: Integer;
begin
  Result := DelimitersVert.Count;
end;

function TdxCustomContainerReportLink.GetDelimitersVertItem(Index: Integer): Integer;
begin
  Result := Integer(DelimitersVert[Index]);
end;

function TdxCustomContainerReportLink.GetDesignWindow: TdxfmCustomContainerDesignWindow;
begin
  Result := inherited DesignWindow as TdxfmCustomContainerDesignWindow;
end;

function TdxCustomContainerReportLink.GetExcludedComponent(Index: Integer): TComponent;
begin
  Result := TComponent(FExcludedComponents.Objects[Index]);
end;

function TdxCustomContainerReportLink.GetExcludedComponentCount: Integer;
begin
  Result := FExcludedComponents.Count;
end;

function TdxCustomContainerReportLink.GetHiddenComponent(Index: Integer): TComponent;
begin
  Result := TComponent(FHiddenComponents.Objects[Index]);
end;

function TdxCustomContainerReportLink.GetHiddenComponentCount: Integer;
begin
  Result := FHiddenComponents.Count;
end;

function TdxCustomContainerReportLink.GetProducerByClass(ProducerClass: TdxPSCustomProducerClass;
  Component: TComponent): TdxPSCustomProducer;
begin
  Result := ProducerCache[ProducerClass, Component];
end;

function TdxCustomContainerReportLink.GetRootCell: TdxReportCell;
begin
  Result := ReportCells.Cells;
end;

function TdxCustomContainerReportLink.GetTopLevelContainer: TWinControl;
begin
  if IsAggregated then
    Result := Controller.TopLevelContainer
  else
    Result := Container;
end;

procedure TdxCustomContainerReportLink.SetController(Value: TdxCustomContainerReportLink);
begin
  inherited Controller := Value;
end;

procedure TdxCustomContainerReportLink.SetOnCustomDrawItem(Value: TdxContainerReportLinkCustomDrawItemEvent);
begin
  if @FOnCustomDrawItem <> @Value then
  begin
    FOnCustomDrawItem := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TdxCustomContainerReportLink.SetOptionsBehavior(Value: TdxCustomContainerReportLinkOptionsBehavior);
begin
  OptionsBehavior.Assign(Value);
end;

procedure TdxCustomContainerReportLink.SetOptionsDesignerTabs(Value: TdxCustomContainerReportLinkOptionsDesignerTabs);
begin
  OptionsDesignerTabs.Assign(Value);
end;
{.2}
{procedure TdxCustomContainerReportLink.SetOptionsItemPlace(Value: TdxCustomContainerReportLinkOptionsItemPlace);
begin
  OptionsItemPlace.Assign(Value);
end;}

procedure TdxCustomContainerReportLink.SetOptionsPagination(Value: TdxCustomContainerReportLinkOptionsPagination);
begin
  OptionsPagination.Assign(Value);
end;

procedure TdxCustomContainerReportLink.SetOptionsRefinements(Value: TdxCustomContainerReportLinkOptionsRefinements);
begin
  OptionsRefinements.Assign(Value);
end;

procedure TdxCustomContainerReportLink.SetOptionsTransparent(Value: TdxCustomContainerReportLinkOptionsTransparent);
begin
  OptionsTransparent.Assign(Value);
end;

procedure TdxCustomContainerReportLink.SetSupportedCustomDraw(Value: Boolean);
begin
  if FSupportedCustomDraw <> Value then
  begin
    FSupportedCustomDraw := Value;
    LinkModified(True);
  end;
end;

procedure TdxCustomContainerReportLink.LoadAggregatedReportLinks;
var
  I: Integer;
  Component: TComponent;
begin
  if Owner <> nil then
    for I := AggregatedReportLinkCount - 1 downto 0 do
    begin
      Component := Owner.FindComponent(FAggregatedReportLinks[I]);
      if Component <> nil then
        FAggregatedReportLinks.Objects[I] := Component
      else
        FAggregatedReportLinks.Delete(I);
    end;
end;

procedure TdxCustomContainerReportLink.ReadAggregatedReportLinks(Stream: TStream);
begin
  FAggregatedReportLinks.LoadFromStream(Stream);
end;

procedure TdxCustomContainerReportLink.WriteAggregatedReportLinks(Stream: TStream);
begin
  FAggregatedReportLinks.SaveToStream(Stream);
end;

procedure TdxCustomContainerReportLink.LoadExcludedComponents;
var
  I: Integer;
  Component: TComponent;
begin
  if Owner <> nil then
    for I := ExcludedComponentCount - 1 downto 0 do
    begin
      Component := Owner.FindComponent(FExcludedComponents[I]);
      if Component <> nil then
        FExcludedComponents.Objects[I] := Component
      else
        FExcludedComponents.Delete(I);
    end;
end;

procedure TdxCustomContainerReportLink.ReadExcludedComponents(Stream: TStream);
begin
  FExcludedComponents.LoadFromStream(Stream);
end;

procedure TdxCustomContainerReportLink.WriteExcludedComponents(Stream: TStream);
begin
  FExcludedComponents.SaveToStream(Stream);
end;

procedure TdxCustomContainerReportLink.LoadHiddenComponents;
var
  I: Integer;
  Component: TComponent;
begin
  if Owner <> nil then
    for I := HiddenComponentCount - 1 downto 0 do
    begin
      Component := Owner.FindComponent(FHiddenComponents[I]);
      if Component <> nil then
        FHiddenComponents.Objects[I] := Component
      else
        FHiddenComponents.Delete(I);
    end;
end;

procedure TdxCustomContainerReportLink.ReadHiddenComponents(Stream: TStream);
begin
  FHiddenComponents.LoadFromStream(Stream);
end;

procedure TdxCustomContainerReportLink.WriteHiddenComponents(Stream: TStream);
begin
  FHiddenComponents.SaveToStream(Stream);
end;

{ TdxPSContainerDesignWindow }

constructor TdxfmCustomContainerDesignWindow.Create(AOwner: TComponent);
begin
  inherited;
  tvControls.OnAdvancedCustomDrawItem := TreeView_AdvancedCustomDrawItem;
  tvHiddenControls.OnAdvancedCustomDrawItem := TreeView_AdvancedCustomDrawItem;
  FLastActiveTab := PageControl.ItemIndex;
end;

destructor TdxfmCustomContainerDesignWindow.Destroy;
begin
  TreeView_FreeNodeObjects(tvControls.InnerTreeView);
  TreeView_FreeNodeObjects(tvHiddenControls.InnerTreeView);
  inherited;
end;

procedure TdxfmCustomContainerDesignWindow.BeforeConstruction;
begin
  inherited;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmCustomContainerDesignWindow.DoInitialize;
begin
  inherited;

  dxPSUtl.dxCreateCheckMarkImages(ilControls);
  RefreshHiddenControlsTree;
  RefreshControlsTree;
  RefreshReportLinksList;

  chbxPaginateByControlDetails.Checked := ReportLink.OptionsPagination.ControlDetails;
  chbxPaginateByControls.Checked := ReportLink.OptionsPagination.Controls;
  lichbxPaginateByGroups.Visible := False;
  lichbxPaginateByItems.Visible := False;

  chbxTransparentContainers.Checked := ReportLink.OptionsTransparent.Containers;
  chbxTransparentControls.Checked := ReportLink.OptionsTransparent.Controls;
  chbxTransparentRoot.Checked := ReportLink.OptionsTransparent.Root;
  chbxTransparentGraphics.Checked := ReportLink.OptionsTransparent.Graphics;
  lichbxTransparentGroups.Visible := False;
  lichbxTransparentItems.Visible := False;

  pnlAvailableLinks.Visible := ReportLink.IsDesigning;
  pnlMoveButtonsSite.Visible := ReportLink.IsDesigning;
  lblAggregatedLinks.CaptionOptions.Visible := ReportLink.IsDesigning;
  if not ReportLink.IsDesigning then
  begin
    lbxAggregatedLinks.DragMode := dmManual;
    lbxAggregatedLinks.MultiSelect := False;
    libtnLinksRemoveInconsistents.Visible := False;
  end;

  lilblSize.Visible := False;
  liimgSize.Visible := False;
  lichbxAutoWidth.Visible := False;
end;

procedure TdxfmCustomContainerDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadIconFromResourceEx(imgExpanding, IDB_DXPSGROUPICON_TRANSPARENTS);
  dxLoadIconFromResourceEx(imgPagination, IDB_DXPSGROUPICON_PAGINATION);
  dxLoadIconFromResourceEx(imgGroups, IDB_DXPSGROUPICON_GROUPS);
  dxLoadIconFromResourceEx(imgTabs, IDB_DXPSGROUPICON_TABS);
end;

procedure TdxfmCustomContainerDesignWindow.LoadStrings;
begin
  inherited;
  tshOptions.Caption := cxGetResourceString(@sdxOptions);
  lblTransparents.Caption := cxGetResourceString(@sdxTransparents);
  chbxTransparentContainers.Caption := cxGetResourceString(@sdxContainers);
  chbxTransparentControls.Caption := cxGetResourceString(@sdxControls);
  chbxTransparentRoot.Caption := cxGetResourceString(@sdxRoot);
  chbxTransparentGraphics.Caption := cxGetResourceString(@sdxGraphics);
  chbxTransparentGroups.Caption := cxGetResourceString(@sdxGroups);
  chbxTransparentItems.Caption := cxGetResourceString(@sdxItems);

  (*lblControlsPlace.Caption := cxGetResourceString(@sdxControlsPlace);
  chbxExpandHeight.Caption := cxGetResourceString(@sdxExpandHeight);
  chbxExpandWidth.Caption := cxGetResourceString(@sdxExpandWidth);
  chbxShrinkHeight.Caption := cxGetResourceString(@sdxShrinkHeight);
  chbxShrinkWidth.Caption := cxGetResourceString(@sdxShrinkWidth);*)

  lblPagination.Caption := cxGetResourceString(@sdxPagination);
  chbxPaginateByControlDetails.Caption := cxGetResourceString(@sdxPaginateByControlDetails);
  chbxPaginateByControls.Caption := cxGetResourceString(@sdxPaginateByControls);
  chbxPaginateByGroups.Caption := cxGetResourceString(@sdxPaginateByGroups);
  chbxPaginateByItems.Caption := cxGetResourceString(@sdxPaginateByItems);

  lblSize.Caption := cxGetResourceString(@sdxSize);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);

  tshReportLinks.Caption := cxGetResourceString(@sdxReportLinksTab);
  pnlAvailableLinks.Caption := cxGetResourceString(@sdxAvailableLinks);
  lblAggregatedLinks.Caption := cxGetResourceString(@sdxAggregatedLinks);
  btnLinksDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  btnLinksRemoveInconsistents.Caption := cxGetResourceString(@sdxBtnRemoveInconsistents);

  tshControls.Caption := cxGetResourceString(@sdxControlsTab);
  btnControlsCheckAll.Caption := cxGetResourceString(@sdxCheckAll);
  btnControlsExpandAll.Caption := cxGetResourceString(@sdxExpandAll);

  tshHiddenControls.Caption := cxGetResourceString(@sdxHiddenControlsTab);
  btnHiddenControlsCheckAll.Caption := cxGetResourceString(@sdxCheckAll);
  btnHiddenControlsExpandAll.Caption := cxGetResourceString(@sdxExpandAll);

  miControlsCheckAll.Caption := cxGetResourceString(@sdxCheckAll);
  miControlsCheckAllChildren.Caption := cxGetResourceString(@sdxCheckAllChildren);
  miControlsUncheckAllChildren.Caption := cxGetResourceString(@sdxUncheckAllChildren);
  miControlsExpandAll.Caption := cxGetResourceString(@sdxExpandAll);

  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviorsTab);
  lbGroups.Caption := cxGetResourceString(@sdxBehaviorsGroups);
  chbxExpandedGroups.Caption := cxGetResourceString(@sdxExpandedGroups);
  chbxSkipEmptyGroups.Caption := cxGetResourceString(@sdxSkipEmptyGroups);
  lbTabs.Caption := cxGetResourceString(@sdxTabs);
  chbxUnwrapTabs.Caption := cxGetResourceString(@sdxUnwrap);
  chbxRiseActiveTabOntoTop.Caption := cxGetResourceString(@sdxActiveTabToTop);
end;

procedure TdxfmCustomContainerDesignWindow.UpdateControlsState;
var
  Root: TTreeNode;
begin
  inherited;
  sbtnAdd.Enabled := CanAggregate;
  sbtnRemove.Enabled := CanDisaggregate;
  btnLinksDesign.Enabled := CanDesign;
  btnLinksRemoveInconsistents.Enabled := CanRemoveInconsistents;

  Root := TreeView_GetRoot(tvControls.InnerTreeView);
  tvControls.Enabled := Root <> nil;
  btnControlsCheckAll.Enabled := (Root <> nil) and TreeView_HasUncheckedChildren(Root);
  btnControlsExpandAll.Enabled := (Root <> nil) and (Root.Count <> 0);

  Root := TreeView_GetRoot(tvHiddenControls.InnerTreeView);
  tvHiddenControls.Enabled := Root <> nil;
  btnHiddenControlsCheckAll.Enabled := (Root <> nil) and TreeView_HasUncheckedChildren(Root);
  btnHiddenControlsExpandAll.Enabled := (Root <> nil) and (Root.Count <> 0);
  tshHiddenControls.Visible := IsDesigning;
end;

function TdxfmCustomContainerDesignWindow.GetActiveTreeView: TcxTreeView;
begin
  if PageControl.ItemIndex = tshControls.Index then
    Result := tvControls
  else
    Result := tvHiddenControls;
end;

procedure TdxfmCustomContainerDesignWindow.InitializeControlsTree;
var
  Root: TTreeNode;
begin
  ReportLink.LoadControlsTree(tvControls.InnerTreeView);

  Root := TreeView_GetRoot(tvControls.InnerTreeView);
  if Root <> nil then
  begin
    TreeView_NormalizeNode(Root);
    Root.Expand(False);
  end;
end;

procedure TdxfmCustomContainerDesignWindow.InitializeHiddenControlsTree;
var
  Root: TTreeNode;
begin
  ReportLink.LoadHiddenControlsTree(tvHiddenControls.InnerTreeView);

  Root := TreeView_GetRoot(tvHiddenControls.InnerTreeView);
  if Root <> nil then
  begin
    TreeView_NormalizeNode(Root);
    Root.Expand(False);
  end;
end;

function TdxfmCustomContainerDesignWindow.IsBoldNode(ANode: TTreeNode): Boolean;
begin
  Result := not TreeView_IsNodeEditable(ANode);
end;

procedure TdxfmCustomContainerDesignWindow.RefreshControlsTree;
begin
  if not (dwsInitialize in State) then tvControls.Items.BeginUpdate;
  try
    InitializeControlsTree;
  finally
    if not (dwsInitialize in State) then tvControls.Items.EndUpdate;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.RefreshHiddenControlsTree;
begin
  if not (dwsInitialize in State) then tvHiddenControls.Items.BeginUpdate;
  try
    InitializeHiddenControlsTree;
  finally
    if not (dwsInitialize in State) then tvHiddenControls.Items.EndUpdate;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.RefreshReportLinksList;
begin
  if ReportLink.IsDesigning then RefreshAvailableLinks;
  RefreshAggregatedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.SetOptionsGroupsByIndex(AnIndex: Integer; AValue: Boolean);
begin
end;

procedure TdxfmCustomContainerDesignWindow.SetOptionsPaginationByIndex(AnIndex: Integer;
  AValue: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ReportLink.OptionsPagination do
    case AnIndex of
      0: ControlDetails := AValue;
      1: Controls := AValue;
    end;
  Modified := True;
end;

procedure TdxfmCustomContainerDesignWindow.SetOptionsSizeByIndex(AnIndex: Integer;
  AValue: Boolean);
begin
end;

procedure TdxfmCustomContainerDesignWindow.SetOptionsTabsByIndex(AnIndex: Integer; AValue: Boolean);
begin
end;

{.2}
{procedure TdxfmCustomContainerDesignWindow.SetOptionsPlaceByIndex(AnIndex: Integer;
  AValue: Boolean);
begin
  if LockControlsUpdate then Exit;

  with ContainerReportLink.OptionsItemPlace do
    case AnIndex of
      0: ExpandHeight := AValue;
      1: ExpandWidth := AValue;
      2: ShrinkHeight := AValue;
      3: ShrinkWidth := AValue;
    end;
  Modified := True;
end;}

procedure TdxfmCustomContainerDesignWindow.SetOptionsTransparentByIndex(AnIndex: Integer;
  AValue: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsTransparent do
    case AnIndex of
      0: Root := AValue;
      1: Controls := AValue;
      2: Containers := AValue;
      3: Graphics := AValue;
    end;
  Modified := True;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_AdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);

  procedure DrawNodeExtraText;
  const
    HighlightColor = clBlue;
  var
    S: string;
    R: TRect;
    TextColor, BkColor: COLORREF;
    X, Y: Integer;
  begin
    with Sender.Canvas do
    begin
      S := '[' + TreeView_GetNodeObject(Node).Component.ClassName + ']';
      R := Node.DisplayRect(True);
      with R do
      begin
        Left := Right;
        Inc(Right, 2 + TextWidth(S) + 2);
        X := Left + 1;
        Y := Top + (Bottom - Top - TextHeight(S)) div 2;
      end;

      TextColor := SetTextColor(Handle, ColorToRGB(HighlightColor));
      BkColor := SetBkColor(Handle, ColorToRGB(clWindow));
      ExtTextOut(Handle, X, Y, ETO_OPAQUE, @R, PChar(S), Length(S), nil);
      SetBkColor(Handle, BkColor);
      SetTextColor(Handle, TextColor);
    end;
  end;

begin
  if TBasedxReportLinkAccess(ReportLink).IsDesigning and (Stage = cdPostPaint) and
    (TreeView_GetNodeObject(Node) <> nil) then
  begin
    DrawNodeExtraText;
    DefaultDraw := False;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_BeginUpdate;
begin
  dxPSCore.dxPSStartWait;
  ActiveTreeView.Items.BeginUpdate;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_CheckAllChildren(ANode: TTreeNode;
  AChecked: Boolean);

  procedure DoCheckAllChildren(ANode: TTreeNode);
  const
    StateMap: array[Boolean, Boolean] of TCheckBoxState = ((cbGrayed, cbChecked), (cbUnchecked, cbChecked));
  var
    I: Integer;
    Node: TTreeNode;
  begin
    for I := 0 to ANode.Count - 1 do
    begin
      Node := ANode[I];
      TreeView_SetNodeState(Node, StateMap[TreeView_IsNodeEditable(Node), AChecked]);
      DoCheckAllChildren(Node);
    end;
  end;

begin
  DoCheckAllChildren(ANode);
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_EndUpdate;
begin
  ActiveTreeView.Items.EndUpdate;
  dxPSCore.dxPSStopWait;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_FreeNodeObjects(ATreeView: TTreeView);

  procedure FreeNodeObject(ANode: TTreeNode);
  var
    I: Integer;
  begin
    TObject(ANode.Data).Free;
    for I := 0 to ANode.Count - 1 do
      FreeNodeObject(ANode[I]);
  end;

var
  Root: TTreeNode;
begin
  Root := TreeView_GetRoot(ATreeView);
  if Root <> nil then
    FreeNodeObject(Root);
end;

function TdxfmCustomContainerDesignWindow.TreeView_GetNodeObject(ANode: TTreeNode): TdxNodeObject;
begin
  Result := TdxNodeObject(ANode.Data);
end;

function TdxfmCustomContainerDesignWindow.TreeView_getRoot(ATreeView: TTreeView): TTreeNode;
begin
  Result := ATreeView.Items.GetFirstNode;
end;

function TdxfmCustomContainerDesignWindow.TreeView_HasCheckedChildren(ANode: TTreeNode): Boolean;
var
  I: Integer;
  Node: TTreeNode;
begin
  Result := True;
  for I := 0 to ANode.Count - 1 do
  begin
    Node := ANode[I];
    if TreeView_GetNodeObject(Node).State <> cbUnchecked then
      Exit;
    if (Node.Count <> 0) and TreeView_HasCheckedChildren(Node) then
      Exit;
  end;
  Result := False;
end;

function TdxfmCustomContainerDesignWindow.TreeView_HasRoot(ATreeView: TTreeView): Boolean;
begin
  Result := TreeView_GetRoot(ATreeView) <> nil;
end;

function TdxfmCustomContainerDesignWindow.TreeView_HasUncheckedChildren(ANode: TTreeNode): Boolean;
var
  I: Integer;
  Node: TTreeNode;
begin
  Result := True;
  for I := 0 to ANode.Count - 1 do
  begin
    Node := ANode[I];
    if TreeView_GetNodeObject(Node).State <> cbChecked then
      Exit;
    if (Node.Count <> 0) and TreeView_HasUncheckedChildren(Node) then
      Exit;
  end;
  Result := False;
end;

function TdxfmCustomContainerDesignWindow.TreeView_IsNodeEditable(ANode: TTreeNode): Boolean;
begin
  Result := (ANode <>nil) and ReportLink.IsComponentEditable(TreeView_GetNodeObject(ANode).Component);
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_NormalizeNode(ANode: TTreeNode);

  procedure DoUpdateNodeState(ANode: TTreeNode);
  var
    NodeObject: TdxNodeObject;
  begin
    NodeObject := TreeView_GetNodeObject(ANode);
    if (NodeObject.State = cbChecked) and TreeView_HasUncheckedChildren(ANode) then
      NodeObject.State := cbGrayed;
    TreeView_UpdateNodeImage(ANode);
  end;

var
  I: Integer;
  Node: TTreeNode;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    Node := ANode[I];
    if Node.Count <> 0 then
      TreeView_NormalizeNode(Node);
  end;
  DoUpdateNodeState(ANode);
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_SetNodeState(ANode: TTreeNode;
  AState: TCheckBoxState);
var
  NodeObject: TdxNodeObject;
  Component: TComponent;
begin
  NodeObject := TreeView_GetNodeObject(ANode);
  NodeObject.State := AState;

  Component := NodeObject.Component;
  case NodeObject.State of
    cbUnchecked:
      if ActiveTreeView = tvHiddenControls then
        ReportLink.HideComponent(Component)
      else
        ReportLink.ExcludeComponent(Component);
    cbChecked:
      if ActiveTreeView = tvHiddenControls then
        ReportLink.UnhideComponent(Component)
      else
        ReportLink.UnexcludeComponent(Component);
  end;
  TreeView_UpdateNodeImage(ANode);

  Modified := True;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_ToggleNodeState(ANode: TTreeNode);
const
  StateToggles: array[TCheckBoxState] of TCheckBoxState = (cbChecked, cbUnchecked, cbChecked);
var
  NodeObject: TdxNodeObject;
begin
  if (ANode <> nil) and TreeView_IsNodeEditable(ANode) then
  begin
    NodeObject := TreeView_GetNodeObject(ANode);
    TreeView_SetNodeState(ANode, StateToggles[NodeObject.State]);
    TreeView_CheckAllChildren(ANode, NodeObject.State = cbChecked);
    if ANode.Parent <> nil then
      TreeView_UpdateNodesState(ANode.Parent);
  end;
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_UpdateNodeImage(ANode: TTreeNode);
begin
  ANode.StateIndex := dxPSUtl.dxCheckStateImageIndexMap(TreeView_GetNodeObject(ANode).State);
end;

procedure TdxfmCustomContainerDesignWindow.TreeView_UpdateNodesState(ANode: TTreeNode);

  procedure DoUpdateNodeState(ANode: TTreeNode);
  begin
    with TreeView_GetNodeObject(ANode) do
      case State of
        cbUnchecked,
        cbChecked:
          if TreeView_HasUncheckedChildren(ANode) then
            State := cbGrayed;
        cbGrayed:
          if not TreeView_HasUncheckedChildren(ANode) then
            State := cbChecked;
      end;
    TreeView_UpdateNodeImage(ANode);
  end;

begin
  DoUpdateNodeState(ANode);
  if ANode.Parent <> nil then
    TreeView_UpdateNodesState(ANode.Parent);
end;

function TdxfmCustomContainerDesignWindow.GetAggregatedLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(lbxAggregatedLinks.Items.Objects[Index]);
end;

function TdxfmCustomContainerDesignWindow.GetAggregatedLinkCount: Integer;
begin
  Result := lbxAggregatedLinks.Items.Count;
end;

function TdxfmCustomContainerDesignWindow.GetAggregatedLinkSelected(Index: Integer): Boolean;
begin
  Result := lbxAggregatedLinks.Selected[Index];
end;

function TdxfmCustomContainerDesignWindow.GetAvailableLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(lbxAvailableLinks.Items.Objects[Index]);
end;

function TdxfmCustomContainerDesignWindow.GetAvailableLinkCount: Integer;
begin
  Result := lbxAvailableLinks.Items.Count;
end;

function TdxfmCustomContainerDesignWindow.GetAvailableLinkSelected(Index: Integer): Boolean;
begin
  Result := lbxAvailableLinks.Selected[Index];
end;

function TdxfmCustomContainerDesignWindow.GetReportLink: TdxCustomContainerReportLink;
begin
  Result := inherited ReportLink as TdxCustomContainerReportLink;
end;

function TdxfmCustomContainerDesignWindow.GetSelectedReportLink: TBasedxReportLink;
begin
  with lbxAggregatedLinks do
    if (not ReportLink.IsDesigning or (SelCount = 1)) and (ItemIndex <> -1) then
      Result := TBasedxReportLink(Items.Objects[ItemIndex])
    else
      Result := nil
end;

procedure TdxfmCustomContainerDesignWindow.DoCheckAllChildren(ANode: TTreeNode;
  AChecked: Boolean);
begin
  if ANode <> nil then
  begin
    TreeView_BeginUpdate;
    try
      TreeView_CheckAllChildren(ANode, AChecked);
      TreeView_UpdateNodesState(ANode);
    finally
      TreeView_EndUpdate;
    end;
    FAreHiddenControlsChanged := True;
    Modified := True;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.DoToggleNodeState(ANode: TTreeNode);
begin
  if TreeView_IsNodeEditable(ANode) then
  begin
    TreeView_BeginUpdate;
    try
      TreeView_ToggleNodeState(ANode);
    finally
      TreeView_EndUpdate;
    end;
    FAreHiddenControlsChanged := True;
    //UpdateControlsState;
    Modified := True;
  end;
end;

function TdxfmCustomContainerDesignWindow.CanAggregate: Boolean;
begin
  Result := lbxAvailableLinks.SelCount <> 0;
end;

function TdxfmCustomContainerDesignWindow.CanDesign: Boolean;
begin
  Result := (SelectedReportLink <> nil) and SelectedReportLink.CheckToDesign;
end;

function TdxfmCustomContainerDesignWindow.CanDisaggregate: Boolean;
begin
  Result := ReportLink.IsDesigning and (lbxAggregatedLinks.SelCount <> 0);
end;

function TdxfmCustomContainerDesignWindow.CanRemoveInconsistents: Boolean;
begin
  Result := ReportLink.HasInconsistentlyAggregatedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.DoAggregateSelectedLinks;
var
  I: Integer;
begin
  for I := 0 to AvailableLinkCount - 1 do
    if AvailableLinkSelected[I] then
      ReportLink.AggregateLink(AvailableLinks[I]);

  RefreshReportLinksList;
  UpdateControlsState;
  Modified := True;
end;

procedure TdxfmCustomContainerDesignWindow.DoDisaggregateSelectedLinks;
var
  I: Integer;
begin
  for I := 0 to AggregatedLinkCount - 1 do
    if AggregatedLinkSelected[I] then
      ReportLink.DisaggregateLink(AggregatedLinks[I]);

  RefreshReportLinksList;
  UpdateControlsState;
  Modified := True;
end;

procedure TdxfmCustomContainerDesignWindow.RefreshAvailableLinks;
var
  I: Integer;
  Link: TBasedxReportLink;
begin
  with lbxAvailableLinks do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to ReportLink.ComponentPrinter.LinkCount - 1 do
      begin
        Link := ReportLink.ComponentPrinter[I];
        if ReportLink.IsLinkAggregable(Link) and not ReportLink.IsLinkAggregated(Link) then
          Items.AddObject(Link.Name, Link);
      end;
      ItemIndex := Items.Count - 1;
      if ItemIndex <> -1 then
        Selected[ItemIndex] := True;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.RefreshAggregatedLinks;
var
  I, AIndex: Integer;
  Link: TBasedxReportLink;
begin
  lbxAggregatedLinks.Items.BeginUpdate;
  try
    lbxAggregatedLinks.Items.Clear;
    for I := 0 to ReportLink.AggregatedReportLinkCount - 1 do
    begin
      Link := ReportLink.AggregatedReportLinks[I];
      lbxAggregatedLinks.Items.AddObject(Link.Name, Link);
    end;
    AIndex := lbxAggregatedLinks.Items.Count - 1;
    if AIndex <> -1 then
      lbxAggregatedLinks.Selected[AIndex] := True;
    lbxAggregatedLinks.ItemIndex := AIndex;
  finally
    lbxAggregatedLinks.Items.EndUpdate;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.DoSelectAll(AListBox: TcxListBox);
begin
  AListBox.SelectAll;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewClick(Sender: TObject);
var
  TreeView: TcxTreeView;
  Pt: TPoint;
  HitTest: THitTests;
begin
  TreeView := TcxTreeView(Sender);
  Pt := TreeView.ScreenToClient(GetMouseCursorPos);
  HitTest := TreeView.GetHitTestInfoAt(Pt.X, Pt.Y);
  if htOnStateIcon in HitTest then
    DoToggleNodeState(TreeView.GetNodeAt(Pt.X, Pt.Y));
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
  begin
    DoToggleNodeState(TcxTreeView(Sender).Selected);
    Key := 0;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #32 then Key := #0;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  Node: TTreeNode;
begin
  if Button = mbRight then
  begin
    Pt := Point(X, Y);
    Node := TcxTreeView(Sender).GetNodeAt(Pt.X, Pt.Y);
    if Node <> nil then
    begin
      Node.Focused := True;
      Node.Selected := True;
    end;
    dxPSPopupMan.dxPSPopupMenuController.ShowPopupAtMousePos(nil, pmControls);
  end;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewCheckAllClick(Sender: TObject);
begin
  DoCheckAllChildren(TreeView_GetRoot(ActiveTreeView.InnerTreeView), True);
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewCheckAllChildrenClick(Sender: TObject);
begin
  DoCheckAllChildren(ActiveTreeView.Selected, True);
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewUncheckAllChildrenClick(Sender: TObject);
begin
  DoCheckAllChildren(ActiveTreeView.Selected, False);
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewExpandAllClick(Sender: TObject);
begin
  ActiveTreeView.FullExpand;
end;

procedure TdxfmCustomContainerDesignWindow.HideStandardControlsClick(Sender: TObject);
begin
  ReportLink.HideStandardControls;
end;

procedure TdxfmCustomContainerDesignWindow.UnhideStandardControlsClick(Sender: TObject);
begin
  ReportLink.UnhideStandardControls;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
const
  FontColors: array[Boolean] of TColor = (clGrayText, clWindowText);
  SelectedBkColors: array[Boolean] of TColor = (clBtnFace, clHighlight);
  SelectedFontColors: array[Boolean] of TColor = (clGrayText, clHighlightText);
begin
  if (Node = nil) or (TreeView_GetNodeObject(Node) = nil) then Exit;
  if IsBoldNode(Node) then
  begin
    if cdsSelected in State then
    begin
      Sender.Canvas.Brush.Color := SelectedBkColors[Sender.Focused];
      if Sender.Focused then
        Sender.Canvas.Font.Color := SelectedFontColors[TreeView_IsNodeEditable(Node)]
      else
        Sender.Canvas.Font.Color := FontColors[TreeView_IsNodeEditable(Node)];
    end
    else
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := FontColors[TreeView_IsNodeEditable(Node)];
    end;
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold];
  end;
end;

procedure TdxfmCustomContainerDesignWindow.DesignClick(Sender: TObject);
begin
  Modified := CanDesign and SelectedReportLink.DesignReport;
end;

procedure TdxfmCustomContainerDesignWindow.pmControlsPopup(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView_GetRoot(ActiveTreeView.InnerTreeView);
  miControlsCheckAll.Enabled := (Node <> nil) and TreeView_HasUncheckedChildren(Node);
  miControlsExpandAll.Enabled := (Node <> nil) and (Node.Count <> 0);

  Node := ActiveTreeView.Selected;
  miControlsCheckAllChildren.Enabled := (Node <> nil) and (Node.Count <> 0) and TreeView_HasUncheckedChildren(Node);
  miControlsUncheckAllChildren.Enabled := (Node <> nil) and (Node.Count <> 0) and TreeView_HasCheckedChildren(Node);
end;

procedure TdxfmCustomContainerDesignWindow.lbxAggregatedLinksClick(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TdxfmCustomContainerDesignWindow.TreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  //UpdateControlsState;
end;

procedure TdxfmCustomContainerDesignWindow.PageControl1Change(Sender: TObject);
begin
  if ReportLink <> nil then
  begin
    UpdateControlsState;
    FAreHiddenControlsChanged := False;
    FLastActiveTab := PageControl.ItemIndex;
    if FAreHiddenControlsChanged and (FLastActiveTab = tshHiddenControls.Index) then
      RefreshControlsTree;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.TransparentClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsTransparentByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmCustomContainerDesignWindow.PaginationClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsPaginationByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmCustomContainerDesignWindow.SizeClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSizeByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmCustomContainerDesignWindow.sbtnAddClick(Sender: TObject);
begin
  DoAggregateSelectedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.sbtnRemoveClick(Sender: TObject);
begin
  DoDisaggregateSelectedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.lbxAvailableLinksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and (ssCtrl in Shift) then
    DoSelectAll(TcxListBox(Sender));
end;

procedure TdxfmCustomContainerDesignWindow.lbxAggregatedLinksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and (ssCtrl in Shift) then
    DoSelectAll(TcxListBox(Sender));
end;

procedure TdxfmCustomContainerDesignWindow.lbxAvailableLinksDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept :=
    (cxExtractDragObjectSource(Source) = lbxAggregatedLinks) and
    (lbxAggregatedLinks.SelCount <> 0);
end;

procedure TdxfmCustomContainerDesignWindow.lbxAvailableLinksDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  DoDisaggregateSelectedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.lbxAggregatedLinksDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept :=
    (cxExtractDragObjectSource(Source) = lbxAvailableLinks) and
    (lbxAvailableLinks.SelCount <> 0);
end;

procedure TdxfmCustomContainerDesignWindow.lbxAggregatedLinksDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  DoAggregateSelectedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.lbxAggregatedLinksDblClick(Sender: TObject);
begin
  if CanDisaggregate then
    DoDisaggregateSelectedLinks
  else
    if CanDesign then btnLinksDesign.Click;
end;

procedure TdxfmCustomContainerDesignWindow.lbxAvailableLinksDblClick(Sender: TObject);
begin
  if CanAggregate then
    DoAggregateSelectedLinks;
end;

procedure TdxfmCustomContainerDesignWindow.btnLinksRemoveInconsistentsClick(Sender: TObject);
begin
  if CanRemoveInconsistents then
  begin
    ReportLink.DisaggregateInconsistentLinks;
    RefreshReportLinksList;
    UpdateControlsState;
  end;
end;

procedure TdxfmCustomContainerDesignWindow.lbxAvailableLinksDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);

  function IsAggregatedLink(ALink: TBasedxReportLink): Boolean;
  begin
    Result := (AControl = lbxAggregatedLinks) and not (ReportLink.IsLinkAggregatedConsistently(ALink) or (odSelected in AState));
  end;

  function IsAvailableLink(ALink: TBasedxReportLink): Boolean;
  begin
    Result := not (odSelected in AState) and ((AControl = lbxAvailableLinks) or ReportLink.IsLinkAggregatedConsistently(ALink));
  end;

var
  R: TRect;
  ALink: TBasedxReportLink;
  AText: string;
begin
  R := ARect;
  ALink := TBasedxReportLink(AControl.Items.Objects[AIndex]);
  ACanvas.FillRect(R, clDefault);
  AText := ALink.ReportDocument.Caption;
  if IsAggregatedLink(ALink) then
    ACanvas.Font.Color := clGrayText;
  ACanvas.DrawTexT(AText, R, cxAlignVCenter);

  if IsDesigning and Assigned(ALink.Component) then
  begin
    Inc(R.Left, ACanvas.TextWidth(AText) + ScaleFactor.Apply(5));
    if IsAvailableLink(ALink) then
      ACanvas.Font.Color := clBlue;
    ACanvas.DrawTexT('[' + ALink.Component.Name + ']', R, cxAlignVCenter);
  end;
end;

procedure TdxfmCustomContainerDesignWindow.GroupsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsGroupsByIndex(TTagToInt(Tag), Checked);
end;

procedure TdxfmCustomContainerDesignWindow.TabsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsTabsByIndex(TTagToInt(Tag), Checked);
end;

procedure RegisterAssistants;
begin
  TdxPSContainerBuilder.Register;

  TdxPSNativePrintableControlProducer.Register;

  TdxPSControlAsMetafileProducer.Register;
  TdxPSWinControlAsMetafileProducer.Register;

  TdxPSCustomContainerItemProducer.Register;
  TdxPSContainerControlProducer.Register;
  TdxPSBevelProducer.Register;
  TdxPSPaintBoxProducer.Register;
  TdxPSShapeProducer.Register;
  TdxPSCustomLabelProducer.Register;
  TdxPSContainerCustomWinControlProducer.Register;
  TdxPSContainerWinControlProducer.Register;
  TdxPSDateTimePickerProducer.Register;
  TdxPSCustomHotKeyProducer.Register;
  TdxPSCustomStaticTextProducer.Register;
  TdxPSCustomEditProducer.Register;
  TdxPSCustomMemoProducer.Register;
  TdxPSCustomComboBoxProducer.Register;
  TdxPSCustomCheckBoxProducer.Register;
  TdxPSRadioButtonProducer.Register;

  TdxPSCustomPanelProducer.Register;
  TdxPSCustomGroupBoxProducer.Register;
  TdxPSCustomRadioGroupProducer.Register;

  TdxPSTabControlProducer.Register;
  TdxPSTabSheetProducer.Register;
  TdxPSPageControlProducer.Register;
  TdxPSNotebookPageProducer.Register;
  TdxPSNotebookProducer.Register;
  TdxPSTabbedNotebookPageProducer.Register;
  TdxPSTabbedNotebookProducer.Register;
  TdxPSScrollingWinControlProducer.Register;
  TdxPScxScrollBoxProducer.Register;
  TdxPSCustomFormProducer.Register;
  TdxPSCustomFrameProducer.Register;
  TdxPSCustomComboBoxExProducer.Register;
end;

procedure RegisterItems;
begin
  TdxReportWinControlHost.Register;
  TdxReportNativePrintableControlHost.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSNativePrintableControlProducerFactory.ReleaseInstance;
  TdxPSContainerItemProducerFactory.ReleaseInstance;
  TdxPSContainerBuilderFactory.ReleaseInstance;
end;

procedure UnregisterItems;
begin
  TdxReportNativePrintableControlHost.Unregister;
  TdxReportWinControlHost.Unregister;
end;

initialization
  RegisterAssistants;
  RegisterItems;
  dxPSRegisterContainers(StandardContainers);

finalization
  dxPSUnregisterContainers(StandardContainers);

  UnregisterItems;
  UnregisterAssistants;

  FreeAndNil(FInternalComponentPrinter);

end.
