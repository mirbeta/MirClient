{*************************************************************************}
{ TMS TAdvCustomFilterPanel component                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2014 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}
unit AdvCustomFilterPanel;

interface

uses
  Classes, Windows, StdCtrls, ExtCtrls, Controls, SysUtils, Dialogs, Forms,
  AdvGrid, ComCtrls, Buttons, AdvUtil ,Spin, Graphics, DBGrids, Generics.Collections,
  DB, RTTI, AdvFilterPanelButton, GDIPicture;

{$R AdvCustomFilterPanel.res}

const
  tagColumn = 1;
  tagOperation = 2;
  tagValue = 3;
  tagCase = 4;
  tagAction = 5;

  tagClear = 3;
  tagAdd = 2;
  tagRestore = 1;

  tagButtonAdd = 1;
  tagButtonRemove = 2;

type
  TAdvCustomFilterPanel = class;

  TAdvFilterUIType = (Desktop,Phone,Auto);

  TFilterOperation = (foEqual, foNotEqual, foLargerThen, foSmallerThen, foContains, foEndsWith, foBeginsWith, foLargerOrEqual, foSmallerOrEqual, foTrueFalse );

  TColumnEditor = (ceText, ceNumeric, ceDate, ceTime, ceBoolean);

  TFilterUI  = class(TPersistent)
  private
    FColumnLabel: string;
    FOperationLabel: string;
    FValueLabel: string;
    FCaseLabel: string;
    FColumnHint: string;
    FOperationHint: string;
    FCaseHint: string;
    FRestoreButton: string;
    FApplyButton: string;
    FClearButton: string;
    FRestoreDialogText: string;
    FApplyDialogText: string;
    FOnChange: TNotifyEvent;
    FOperationEqual: string;
    FOperationNotEqual: string;
    FOperationContains: string;
    FOperationSmallerThen: string;
    FOperationLargerThen: string;
    FOperationSmallerOrEqual: string;
    FOperationLargerOrEqual: string;
    FOperationBeginsWith: string;
    FOperationEndsWith: string;
    FHintFilterAdd: string;
    FHintFilterRemove: string;
    FHintApplyFilter: string;
    FHintClearFilter: string;
    FHintRestoreFilter: string;
    FOperatorAnd: string;
    FOperatorOr: string;
    FValueTextHint: string;
    FOperationTrueFalse: string;
    FClearDialogText: string;
    procedure SetApplyButton(const Value: string);
    procedure SetApplyDialogText(const Value: string);
    procedure SetCaseHint(const Value: string);
    procedure SetCaseLabel(const Value: string);
    procedure SetColumnHint(const Value: string);
    procedure SetColumnLabel(const Value: string);
    procedure SetOperationHint(const Value: string);
    procedure SetOperationLabel(const Value: string);
    procedure SetClearButton(const Value: string);
    procedure SetClearDialogText(const Value: string);
    procedure SetRestoreButton(const Value: string);
    procedure SetRestoreDialogText(const Value: string);
    procedure SetValueLabel(const Value: string);
    procedure SetOperationEqual(const Value: string);
    procedure SetOperationNotEqual(const Value: string);
    procedure SetOperationContains(const Value: string);
    procedure SetOperationSmallerThen(const Value: string);
    procedure SetOperationLargerThen(const Value: string);
    procedure SetOperationSmallerOrEqual(const Value: string);
    procedure SetOperationLargerOrEqual(const Value: string);
    procedure SetOperationBeginsWith(const Value: string);
    procedure SetOperationEndsWith(const Value: string);
    procedure SetHintFilterAdd(const Value: string);
    procedure SetHintFilterRemove(const Value: string);
    procedure SetHintApplyFilter(const Value: string);
    procedure SetHintClearFilter(const Value: string);
    procedure SetHintRestoreFilter(const Value: string);
    procedure SetOperatorAnd(const Value: string);
    procedure SetOperatorOr(const Value: string);
    procedure SetValueTextHint(const Value: string);
    procedure SetOperationTrueFalse(const Value: string);
  protected
    procedure DoChange;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
  published
    property ApplyButton: string read FApplyButton write SetApplyButton;
    property ApplyDialogText: string read FApplyDialogText write SetApplyDialogText;
    property ColumnLabel: string read FColumnLabel write SetColumnLabel;
    property ColumnHint: string read FColumnHint write SetColumnHint;
    property CaseLabel: string read FCaseLabel write SetCaseLabel;
    property CaseHint: string read FCaseHint write SetCaseHint;
    property HintApplyFilter: string read FHintApplyFilter write SetHintApplyFilter;
    property HintClearFilter: string read FHintClearFilter write SetHintClearFilter;
    property HintFilterAdd: string read FHintFilterAdd write SetHintFilterAdd;
    property HintFilterRemove: string read FHintFilterRemove write SetHintFilterRemove;
    property HintRestoreFilter: string read FHintRestoreFilter write SetHintRestoreFilter;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OperationLabel: string read FOperationLabel write SetOperationLabel;
    property OperationHint: string read FOperationHint write SetOperationHint;
    property OperationEqual: string read FOperationEqual write SetOperationEqual;
    property OperationNotEqual: string read FOperationNotEqual write SetOperationNotEqual;
    property OperationContains: string read FOperationContains write SetOperationContains;
    property OperationBeginsWith: string read FOperationBeginsWith write SetOperationBeginsWith;
    property OperationEndsWith: string read FOperationEndsWith write SetOperationEndsWith;
    property OperationSmallerThen: string read FOperationSmallerThen write SetOperationSmallerThen;
    property OperationLargerThen: string read FOperationLargerThen write SetOperationLargerThen;
    property OperationSmallerOrEqual: string read FOperationSmallerOrEqual write SetOperationSmallerOrEqual;
    property OperationLargerOrEqual: string read FOperationLargerOrEqual write SetOperationLargerOrEqual;
    property OperationTrueFalse: string read FOperationTrueFalse write SetOperationTrueFalse;
    property OperatorAnd: string read FOperatorAnd write SetOperatorAnd;
    property OperatorOr: string read FOperatorOr write SetOperatorOr;
    property ClearButton: string read FClearButton write SetClearButton;
    property ClearDialogText: string read FClearDialogText write SetClearDialogText;
    property RestoreButton: string read FRestoreButton write SetRestoreButton;
    property RestoreDialogText: string read FRestoreDialogText write SetRestoreDialogText;
    property ValueLabel: string read FValueLabel write SetValueLabel;
    property ValueTextHint: string read FValueTextHint write SetValueTextHint;
  end;

  TAdvFilterGroupBox = class(TCustomControl)
  private
    FColor: TColor;
    FColorTo: TColor;
    FDirection: Boolean;
    FBorderColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Direction: Boolean read FDirection write SetDirection default false;
  end;

  TAdvItemGradientDirection = (gdHorizontal,gdVertical);

  TAdvFilterContainer = class(TCustomControl)
  private
    FColor: TColor;
    FColorTo: TColor;
    FDirection: Boolean;
    FCaption: string;
    FAlignment: TAlignment;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Color: TColor read FColor write SetColor default clSilver;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Direction: Boolean read FDirection write SetDirection default false;
    property Caption: string read FCaption write SetCaption;
  end;

  TAdvCustomFilterPanel = class(TCustomControl)
  private
    FFilterUI: TFilterUI;
    FButtonAppearance: TFilterButtonAppearance;
    FScrollBox: TScrollBox;
    FCustomControl: TAdvFilterContainer;
    FHeader: TAdvFilterContainer;
    FFooter: TAdvFilterContainer;
    FFooterAddClearButton: TButton;
    FFooterAddFilterButton: TButton;
    FFooterRestoreFilterButton: TButton;
    FLabelFont: TFont;
    FTitleFont: TFont;
    FTitleAlignment: TAlignment;
    FTitle: string;
    FButtonAddIcon: TGDIPPicture;
    FButtonRemoveIcon: TGDIPPicture;
    FItemColorStart: TColor;
    FItemColorEnd: TColor;
    FItemColorDirection: TAdvItemGradientDirection;
    FItemBorderColor: TColor;
    FShowRestoreFilter: Boolean;
    FShowApplyFilter: Boolean;
    FShowClearFilter: Boolean;
    FColorStart: TColor;
    FColorEnd: TColor;
    FColorDirection: TAdvItemGradientDirection;
    FFooterColorStart: TColor;
    FFooterColorEnd: Tcolor;
    FHeaderColorStart: TColor;
    FHeaderColorEnd: TColor;
    FHeaderColorDirection: TAdvItemGradientDirection;
    FBorderStyle: TBorderStyle;
    FFooterColorDirection: TAdvItemGradientDirection;
    FShowCase: Boolean;
    FOnApplyFilter: TNotifyEvent;
    FOnRemoveFilter: TNotifyEvent;
    FOnRestoreFilter: TNotifyEvent;
    FSortColumnNames: boolean;
    FDefaultOperation: TFilterOperation;
    FDefaultColumn: integer;
    procedure SetUI(const Value: TFilterUI);
    procedure ApplyLabelFont;
    procedure SetLabelFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetButtonAddIcon(const Value: TGDIPPicture);
    procedure SetButtonRemoveIcon(const Value: TGDIPPicture);
    procedure SetItemColorStart(const Value: TColor);
    procedure SetItemColorEnd(const Value: TColor);
    procedure SetItemColorDirection(const Value: TAdvItemGradientDirection);
    procedure SetItemBorderColor(const Value: TColor);
    procedure SetShowRestoreFilter(const Value: Boolean);
    procedure SetShowApplyFilter(const Value: Boolean);
    procedure SetShowClearFilter(const Value: Boolean);
    procedure SetColorStart(const Value: TColor);
    procedure SetColorEnd(const Value: TColor);
    procedure SetColorDirection(const Value: TAdvItemGradientDirection);
    procedure SetHeaderColorStart(const Value: TColor);
    procedure SetFooterColorStart(const Value: TColor);
    procedure SetFooterColorEnd(const Value: Tcolor);
    procedure SetHeaderColorEnd(const Value: TColor);
    procedure SetHeaderColorDirection(const Value: TAdvItemGradientDirection);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetVersionNr: integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetFooterColorDirection(const Value: TAdvItemGradientDirection);
    procedure SetTitleAlignment(const Value: TAlignment);
    procedure SetFCustomControl(const Value: TAdvFilterContainer);
    procedure SetButtonAppearance(const Value: TFilterButtonAppearance);
    procedure SetShowCase(const Value: Boolean);
    procedure SetSortColumnNames(const Value: boolean);
    procedure SetDefaultColumn(const Value: integer);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FilterUIChanged(Sender: TObject);
    procedure ButtonAddIconChanged(Sender: TObject);
    procedure ButtonRemoveIconChanged(Sender: TObject);
    procedure LabelFontChanged(Sender: TObject);
    procedure TitleFontChanged(Sender: TObject);
    procedure ButtonAppearanceChanged(Sender: TObject);
    procedure SetCustomControlHeight;
    function GetColumnNames: TStringList; virtual; abstract;
    function GetFilterOperations: TStringList; virtual; abstract;
    procedure AddClickHandler(Sender: TObject); virtual; abstract;
    procedure RemoveClickHandler(Sender: TObject); virtual; abstract;
    procedure AddFilterClick(Sender: TObject); virtual;
    procedure AddClear(Sender: TObject); virtual;
    procedure RestoreFilterClick(Sender: TObject); virtual;
    procedure AddNewRow(position: integer; hasAction: boolean);
    procedure ColumnChangeHandler(Sender: TObject); virtual;
    function GetEditorType(AColumn: integer): TColumnEditor; virtual; abstract;

    function AddBool(groupbox: TCustomControl; AChecked: Boolean = false): TCheckBox;
    procedure AddDate(groupbox: TCustomControl; AText: string);
    function AddEdit(groupbox: TCustomControl; AColumn: integer; AText: string): TControl; virtual;
    procedure AddSpinEdit(groupbox: TCustomControl; AText: string);
    procedure Loaded; override;
    procedure Init; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Header: TAdvFilterContainer read FHeader write FHeader;
    property Footer: TAdvFilterContainer read FFooter write FFooter;
    property FooterAddClearButton: TButton read FFooterAddClearButton write FFooterAddClearButton;
    property FooterAddFilterButton: TButton read FFooterAddFilterButton write FFooterAddFilterButton;
    property FooterRestoreFilterButton: TButton read FFooterRestoreFilterButton write FFooterRestoreFilterButton;
    property CustomControl: TAdvFilterContainer read FCustomControl write SetFCustomControl;
   published
    property Align;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ButtonAppearance: TFilterButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property ButtonAddIcon: TGDIPPicture read FButtonAddIcon write SetButtonAddIcon;
    property ButtonRemoveIcon: TGDIPPicture read FButtonRemoveIcon write SetButtonRemoveIcon;
    property ColorStart: TColor read FColorStart write SetColorStart default clWhite;
    property ColorEnd: TColor read FColorEnd write SetColorEnd default clNone;
    property ColorDirection: TAdvItemGradientDirection read FColorDirection write SetColorDirection default gdVertical;
    property Ctl3D;
    property DefaultOperation: TFilterOperation read FDefaultOperation write FDefaultOperation default foEqual;
    property DefaultColumn: integer read FDefaultColumn write SetDefaultColumn default -1;
    property FooterColorStart: TColor read FFooterColorStart write SetFooterColorStart default clSilver;
    property FooterColorEnd: Tcolor read FFooterColorEnd write SetFooterColorEnd default clNone;
    property FooterColorDirection: TAdvItemGradientDirection read FFooterColorDirection write SetFooterColorDirection default gdVertical;
    property HeaderColorStart: TColor read FHeaderColorStart write SetHeaderColorStart default clGray;
    property HeaderColorEnd: TColor read FHeaderColorEnd write SetHeaderColorEnd default clNone;
    property HeaderColorDirection: TAdvItemGradientDirection read FHeaderColorDirection write SetHeaderColorDirection default gdVertical;
    property ItemBorderColor: TColor read FItemBorderColor write SetItemBorderColor default clGray;
    property ItemColorStart: TColor read FItemColorStart write SetItemColorStart default clSilver;
    property ItemColorEnd: TColor read FItemColorEnd write SetItemColorEnd default clNone;
    property ItemColorDirection: TAdvItemGradientDirection read FItemColorDirection write SetItemColorDirection default gdVertical;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ShowApplyFilter: Boolean read FShowApplyFilter write SetShowApplyFilter default True;
    property ShowCase: Boolean read FShowCase write SetShowCase default True;
    property ShowClearFilter: Boolean read FShowClearFilter write SetShowClearFilter default True;
    property ShowRestoreFilter: Boolean read FShowRestoreFilter write SetShowRestoreFilter default True;
    property ShowHint;
    property SortColumnNames: boolean read FSortColumnNames write SetSortColumnNames default false;
    property TitleAlignment: TAlignment read FTitleAlignment write SetTitleAlignment default taLeftJustify;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property Title: string read FTitle write SetTitle;
    property UI: TFilterUI read FFilterUI write SetUI;
    property Version: string read GetVersion write SetVersion;
    property Visible;

    property OnRestoreFilter: TNotifyEvent read FOnRestoreFilter write FOnRestoreFilter;
    property OnApplyFilter: TNotifyEvent read FOnApplyFilter write FOnApplyFilter;
    property OnRemoveFilter: TNotifyEvent read FOnRemoveFilter write FOnRemoveFilter;
  end;

  TAdvCustomFilterForm = class(TForm)
  private
    FFilterPanel: TAdvCustomFilterPanel;
    FOnApplyFilter: TNotifyEvent;
    FOnRemoveFilter: TNotifyEvent;
    FOnRestoreFilter: TNotifyEvent;
  protected
    procedure CreateWnd; override;
    procedure DoApplyFilter(Sender: TObject);
    procedure DoRemoveFilter(Sender: TObject);
    procedure DoRestoreFilter(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    function CreateFilterPanel(AOwner: TComponent): TAdvCustomFilterPanel; virtual; abstract;
    property FilterPanel: TAdvCustomFilterPanel read FFilterPanel;
    property OnRestoreFilter: TNotifyEvent read FOnRestoreFilter write FOnRestoreFilter;
    property OnApplyFilter: TNotifyEvent read FOnApplyFilter write FOnApplyFilter;
    property OnRemoveFilter: TNotifyEvent read FOnRemoveFilter write FOnRemoveFilter;
  end;

  TAdvCustomGridFilterDialog = class(TComponent)
  private
    FFilterUI: TFilterUI;
    FShowApplyFilter: Boolean;
    FItemColorDirection: TAdvItemGradientDirection;
    FTitleAlignment: TAlignment;
    FFooterColorDirection: TAdvItemGradientDirection;
    FShowClearFilter: Boolean;
    FHeaderColorStart: TColor;
    FColorEnd: TColor;
    FColorDirection: TAdvItemGradientDirection;
    FTitle: string;
    FButtonRemoveIcon: TGDIPPicture;
    FButtonAddIcon: TGDIPPicture;
    FShowRestoreFilter: Boolean;
    FItemColorStart: TColor;
    FItemBorderColor: TColor;
    FFooterColorStart: TColor;
    FHeaderColorEnd: TColor;
    FLabelFont: TFont;
    FHeaderColorDirection: TAdvItemGradientDirection;
    FItemColorEnd: TColor;
    FTitleFont: TFont;
    FFooterColorEnd: Tcolor;
    FColorStart: TColor;
    FCaption: string;
    FShowCase: boolean;
    FForm: TAdvCustomFilterForm;
    FOnApplyFilter: TNotifyEvent;
    FOnRemoveFilter: TNotifyEvent;
    FOnRestoreFilter: TNotifyEvent;
    procedure SetButtonAddIcon(const Value: TGDIPPicture);
    procedure SetButtonRemoveIcon(const Value: TGDIPPicture);
    procedure SetLabelFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetUI(const Value: TFilterUI);
    procedure SetCaption(const Value: string);
  protected
    procedure FormShow(Sender: TObject); virtual;
    function CreateFilterForm: TAdvCustomFilterForm; virtual; abstract;
    property Form: TAdvCustomFilterForm read FForm;
    procedure DoApplyFilter(Sender: TObject);
    procedure DoRemoveFilter(Sender: TObject);
    procedure DoRestoreFilter(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property ButtonAddIcon: TGDIPPicture read FButtonAddIcon write SetButtonAddIcon;
    property ButtonRemoveIcon: TGDIPPicture read FButtonRemoveIcon write SetButtonRemoveIcon;
    property Caption: string read FCaption write SetCaption;
    property ColorStart: TColor read FColorStart write FColorStart default clWhite;
    property ColorEnd: TColor read FColorEnd write FColorEnd default clNone;
    property ColorDirection: TAdvItemGradientDirection read FColorDirection write FColorDirection default gdVertical;
    property FooterColorStart: TColor read FFooterColorStart write FFooterColorStart default clSilver;
    property FooterColorEnd: Tcolor read FFooterColorEnd write FFooterColorEnd default clNone;
    property FooterColorDirection: TAdvItemGradientDirection read FFooterColorDirection write FFooterColorDirection default gdVertical;
    property HeaderColorStart: TColor read FHeaderColorStart write FHeaderColorStart default clGray;
    property HeaderColorEnd: TColor read FHeaderColorEnd write FHeaderColorEnd default clNone;
    property HeaderColorDirection: TAdvItemGradientDirection read FHeaderColorDirection write FHeaderColorDirection default gdVertical;
    property ItemBorderColor: TColor read FItemBorderColor write FItemBorderColor default clGray;
    property ItemColorStart: TColor read FItemColorStart write FItemColorStart default clSilver;
    property ItemColorEnd: TColor read FItemColorEnd write FItemColorEnd default clNone;
    property ItemColorDirection: TAdvItemGradientDirection read FItemColorDirection write FItemColorDirection default gdVertical;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ShowApplyFilter: Boolean read FShowApplyFilter write FShowApplyFilter default True;
    property ShowCase: Boolean read FShowCase write FShowCase default True;
    property ShowClearFilter: Boolean read FShowClearFilter write FShowClearFilter default True;
    property ShowRestoreFilter: Boolean read FShowRestoreFilter write FShowRestoreFilter default True;
    property TitleAlignment: TAlignment read FTitleAlignment write FTitleAlignment default taLeftJustify;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property Title: string read FTitle write FTitle;
    property UI: TFilterUI read FFilterUI write SetUI;
    property OnRestoreFilter: TNotifyEvent read FOnRestoreFilter write FOnRestoreFilter;
    property OnApplyFilter: TNotifyEvent read FOnApplyFilter write FOnApplyFilter;
    property OnRemoveFilter: TNotifyEvent read FOnRemoveFilter write FOnRemoveFilter;

  end;

implementation

const
  containerX = 5;
  containerY = 85;
  containerWidth = 529;
  containerHeight = 55;

  labelColumnX = 8;
  labelColumnY = 10;
  columnWidth = 145;

  labelOperationX = 159;
  labelOperationY = 10;
  operationWidth = 145;

  labelValueX = 310;
  labelValueY = 10;
  valueWidth = 125;

  labelCaseX = 441;
  labelCaseY = 10;
  caseWidth = 13;

  actionX = 5;

{ TAdvCustomFilterPanel }

procedure TAdvCustomFilterPanel.ColumnChangeHandler(Sender: TObject);
var
  cb: TComboBox;
  C: TCustomControl;
  II,column: Integer;
  B: Boolean;
  sl: TStringList;
  I: Integer;
  ce: TColumnEditor;

  function GetDefault(i, c: integer): integer;
  begin
    if i < c then
      Result := i
    else
      Result := 0;
  end;

begin
  B := False;

  //First remove the previous field (if any)
  C := TCustomControl(TComboBox(Sender).Parent);

  // operation comboboxes must be restored
  for II := C.ControlCount - 1 downto 0 do
  begin
    if (C.Controls[II].Tag = tagOperation) and (C.Controls[II] is TComboBox)then
    begin
      sl := GetFilterOperations;
      TComboBox(C.Controls[II]).Items := sl;
      sl.Free;
    end;
  end;

  // loop over all controls inside the Container
  for II := C.ControlCount - 1 downto 0 do
  begin
    // comboboxes
    if (C.Controls[II].Tag = tagValue) and not (C.Controls[II] is TLabel)then
    begin
       C.Controls[II].Free;
    end;
  end;

  if TComboBox(Sender).Text = '' then
    Exit;

  cb := TComboBox(Sender);
  column := Integer(cb.Items.Objects[cb.ItemIndex]);

  ce := GetEditorType(Column);

  case ce of
    ceText:
    begin
      AddEdit(C,Column,'');
      B := True;
    end;
    ceNumeric: AddSpinEdit(C,'');
    ceDate, ceTime: AddDate(C,DateToStr(Date));
    ceBoolean:
    begin
      AddBool(C);
      // loop over all controls inside the C
      for II := C.ControlCount - 1 downto 0 do
      begin
        // operation comboboxes must change
        if (C.Controls[II].Tag = tagOperation) and (C.Controls[II] is TComboBox)then
        begin
          for I := TComboBox(C.Controls[II]).Items.Count - 2 downto 0 do
          begin
            TComboBox(C.Controls[II]).Items.Delete(Integer(I));
            TComboBox(C.Controls[II]).ItemIndex := 0;
          end;
        end;
      end;
    end;
  end;

  if not B then
  begin
    // loop over all controls inside the C
    for II := C.controlCount - 1 downto 0 do
    begin
      // operation comboboxes
      if (C.Controls[II].Tag = tagOperation) and (C.Controls[II] is TComboBox)then
      begin
         TComboBox(C.Controls[II]).Items.Delete(Integer(TFilterOperation.foTrueFalse));
         TComboBox(C.Controls[II]).Items.Delete(Integer(TFilterOperation.foBeginsWith));
         TComboBox(C.Controls[II]).Items.Delete(Integer(TFilterOperation.foEndsWith));
         TComboBox(C.Controls[II]).Items.Delete(Integer(TFilterOperation.foContains));

         if (DefaultOperation in [foTrueFalse, foBeginsWith, foEndsWith, foContains]) then
           TComboBox(C.Controls[II]).ItemIndex := 0
         else
           TComboBox(C.Controls[II]).ItemIndex := GetDefault(integer(DefaultOperation), TComboBox(C.Controls[II]).Items.Count);
      end;

      // It's not a string => do not show the case indicator
      if (C.Controls[II].Tag = tagCase) then
      begin
        C.Controls[II].Visible := false;
      end;
    end;
  end
  else
  begin
    // loop over all controls inside the C
    for II := C.controlCount - 1 downto 0 do
    begin
      // comboboxes
      if (C.Controls[II].Tag = tagOperation) and (C.Controls[II] is TComboBox)then
      begin
        sl := GetFilterOperations;
        TComboBox(C.Controls[II]).Items := sl;
        TComboBox(C.Controls[II]).Items.Delete(Integer(TFilterOperation.foTrueFalse));
        sl.Free;

        if (DefaultOperation = foTrueFalse) then
          TComboBox(C.Controls[II]).ItemIndex := 0
        else
          TComboBox(C.Controls[II]).ItemIndex := GetDefault(integer(DefaultOperation), TComboBox(C.Controls[II]).Items.Count);
      end;

      // It's a string => show the case indicator
      if (C.Controls[II].Tag = tagCase) then
      begin
        C.Controls[II].Visible := true;
      end;
    end;
  end;

end;

constructor TAdvCustomFilterPanel.Create(AOwner: TComponent);
begin
  inherited;

  FFilterUI := TFilterUI.Create(Self);
  FFilterUI.OnChange := FilterUIChanged;

  DoubleBuffered := true;

  BorderStyle := bsSingle;
  Ctl3D := true;

  Width := 670;
  Height := 350;

  Constraints.MinWidth := 670;
  Constraints.MinHeight := 350;

  FShowCase := true;
  FShowRestoreFilter := true;
  FShowApplyFilter := true;
  FShowClearFilter := true;

  FSortColumnNames := false;
  FDefaultOperation := foEqual;
  FDefaultColumn := -1;

  FColorStart := clWhite;
  FColorEnd := clNone;
  FColorDirection := gdVertical;

  FTitleFont := TFont.Create;
  FTitleFont.Color := clWhite;
  FTitleFont.OnChange := TitleFontChanged;

  FHeaderColorStart := clGray;
  FHeaderColorEnd := clNone;
  FHeaderColorDirection := gdVertical;

  FHeader := TAdvFilterContainer.Create(Self);
  FHeader.Height := 25;
  FHeader.Align := alTop;
  FHeader.Text := 'DB Filter';
  FHeader.Parent := Self;
  FHeader.Font.Assign(TitleFont);
  FHeader.Color := FHeaderColorStart;
  FHeader.ColorTo := FHeaderColorEnd;

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.DoubleBuffered := true;
  FScrollBox.Parent := Self;
  FScrollBox.Top := 35;
  FScrollBox.width := Self.Width;
  FScrollBox.Height := Self.Height - 50;
  FScrollBox.BorderStyle := bsNone;
  FScrollBox.Align := alClient;
  FScrollBox.ParentBackground := true;

  FCustomControl := TAdvFilterContainer.Create(Self);
  FCustomControl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  FCustomControl.Height := FScrollBox.Height;
  FCustomControl.Width := FScrollBox.Width;
  FCustomControl.Align := alNone;
  FCustomControl.Parent := FScrollBox;
  FCustomControl.DoubleBuffered := true;
  FCustomControl.Color := FColorStart;
  FCustomControl.ColorTo := FColorEnd;

  FFooterColorStart := clSilver;
  FFooterColorEnd := clNone;
  FFooterColorDirection := gdVertical;

  FFooter := TAdvFilterContainer.Create(Self);
  FFooter.Align := alBottom;
  FFooter.Height := 25;
  FFooter.Parent := Self;
  FFooter.Color := FFooterColorStart;
  FFooter.ColorTo := FFooterColorEnd;


  FLabelFont := TFont.Create;
  FLabelFont.Color := clBlack;
  FLabelFont.OnChange := LabelFontChanged;

  FButtonAddIcon := TGDIPPicture.Create;
  FButtonAddIcon.OnChange := ButtonAddIconChanged;

  FButtonRemoveIcon := TGDIPPicture.Create;
  FButtonRemoveIcon.OnChange := ButtonRemoveIconChanged;

  FButtonAppearance := TFilterButtonAppearance.Create;
  FButtonAppearance.OnChange := ButtonAppearanceChanged;

  FItemBorderColor := clGray;
  FItemColorStart := clSilver;
  FItemColorEnd := clNone;
  FItemColorDirection := gdVertical;

  FShowClearFilter := true;
  FShowRestoreFilter := true;
  FShowApplyFilter := true;

  FTitleAlignment := taLeftJustify;

  FItemColorStart := clSilver;
  FItemColorEnd := clNone;

  FButtonAddIcon.LoadFromResourceName(HInstance, 'ADVCUSTOMFILTERADD');
  FButtonRemoveIcon.LoadFromResourceName(HInstance, 'ADVCUSTOMFILTERREMOVE');

  FooterAddFilterButton := TButton.Create(Self);
  FooterAddFilterButton.Align := alRight;
  FooterAddFilterButton.Caption := FFilterUI.ApplyButton;
  FooterAddFilterButton.OnClick := AddFilterClick;
  FooterAddFilterButton.Tag := tagAdd;
  FooterAddFilterButton.Parent := Footer;
  FooterAddFilterButton.AlignWithMargins := true;

  FooterAddClearButton := TButton.Create(Self);
  FooterAddClearButton.Align := alRight;
  FooterAddClearButton.Caption := FFilterUI.ClearButton;;
  FooterAddClearButton.Parent := Footer;
  FooterAddClearButton.OnClick := AddClear;
  FooterAddClearButton.Tag := tagClear;
  FooterAddClearButton.AlignWithMargins := true;

  FooterRestoreFilterButton := TButton.Create(Self);
  FooterRestoreFilterButton.Align := alRight;
  FooterRestoreFilterButton.Caption := FFilterUI.RestoreButton;
  FooterRestoreFilterButton.Parent := Footer;
  FooterRestoreFilterButton.OnClick := RestoreFilterClick;
  FooterRestoreFilterButton.Tag := tagRestore;
  FooterRestoreFilterButton.AlignWithMargins := true;
end;

procedure TAdvCustomFilterPanel.Loaded;
begin
  inherited;
  FHeader.Font.Assign(TitleFont);
end;

procedure TAdvCustomFilterPanel.SetCustomControlHeight;
var
  cHeight: Integer;
begin
  if Assigned(FCustomControl) then
  begin
    cHeight := FCustomControl.ControlCount * (containerHeight);

    if cHeight > FCustomControl.Height then
      FCustomControl.Height := cHeight;

    if cHeight <= FScrollBox.Height then
      FCustomControl.Height := FScrollBox.Height;

    FCustomControl.Width := FScrollBox.ClientWidth;
  end;
end;

procedure TAdvCustomFilterPanel.SetDefaultColumn(const Value: integer);
begin
  if (Value >= -1) then
  begin
    FDefaultColumn := Value;
    Init;
  end;
end;

procedure TAdvCustomFilterPanel.TitleFontChanged(Sender: TObject);
begin
  FHeader.Font := TitleFont;
end;

procedure TAdvCustomFilterPanel.LabelFontChanged(Sender: TObject);
begin
  ApplyLabelFont;
end;

procedure TAdvCustomFilterPanel.ButtonAddIconChanged(Sender: TObject);
var
  I: Integer;
  C: TCustomControl;
  Y: Integer;
begin
  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if (FCustomControl.Controls[I] is TCustomControl) then
    begin
      C := TCustomControl(FCustomControl.Controls[I]);
      for Y := 0 to C.ControlCount - 1 do
      begin
        if (C.Controls[Y] is TAdvFilterPanelButton) and (C.Controls[Y].Tag = tagButtonRemove) then
        begin
          TAdvFilterPanelButton(C.Controls[Y]).Picture.Assign(ButtonAddIcon);
          Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.ButtonAppearanceChanged(Sender: TObject);
var
  I: Integer;
  C:TCustomControl;
  Y: Integer;
begin
  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if (FCustomControl.Controls[I] is TCustomControl) then
    begin
      C := TCustomControl(FCustomControl.Controls[I]);
      for Y := 0 to C.ControlCount - 1 do
      begin
        if C.Controls[Y] is TAdvFilterPanelButton then
        begin
          TAdvFilterPanelButton(C.Controls[Y]).Appearance.Assign(ButtonAppearance);
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.ButtonRemoveIconChanged(Sender: TObject);
var
  I: Integer;
  C:TCustomControl;
  Y: Integer;
begin
  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if (FCustomControl.Controls[I] is TCustomControl) then
    begin
      C := TCustomControl(FCustomControl.Controls[I]);
      for Y := 0 to C.ControlCount - 1 do
      begin
        if (C.Controls[Y] is TAdvFilterPanelButton) and (C.Controls[Y].Tag = tagButtonRemove) then
        begin
          TAdvFilterPanelButton(C.Controls[Y]).Picture.Assign(ButtonRemoveIcon);
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.FilterUIChanged(Sender: TObject);
var
  I: Integer;
  C: TCustomControl;
  Y: Integer;
  L: TLabel;
  B: TButton;
begin
  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if (FCustomControl.Controls[I] is TCustomControl) then
    begin
      if (FCustomControl.Controls[I] is TCustomControl) then
      begin
        C := TCustomControl(FCustomControl.Controls[I]);

        for Y := 0 to C.ControlCount - 1 do
        begin
          if C.Controls[I] is TLabel then
          begin
            L := TLabel(C.Controls[I]);

            if L.Tag = tagColumn then
            begin
              L.Caption := UI.ColumnLabel;
            end;

            if L.Tag = tagOperation then
            begin
              L.Caption := UI.OperationLabel;
            end;

            if L.Tag = tagValue then
            begin
              L.Caption := UI.ValueLabel
            end;

            if L.Tag = tagCase then
            begin
              L.Caption := UI.CaseLabel;
            end;
          end;
        end;
      end;
    end;
  end;


  for Y := 0 to Footer.ControlCount - 1 do
  begin
    if Footer.Controls[Y] is TButton then
    begin
      B:= TButton(Footer.Controls[Y]);

      if B.Tag = tagAdd then
      begin
        B.Caption := FFilterUI.ApplyButton;
      end;

      if B.Tag = tagClear then
      begin
        B.Caption := FFilterUI.ClearButton;
      end;

      if B.Tag = tagRestore then
      begin
        B.Caption := FFilterUI.RestoreButton;
      end;
    end;
  end;
end;

const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  Alignments: array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);

procedure TAdvCustomFilterPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BorderStyles[FBorderStyle];
end;

procedure TAdvCustomFilterPanel.Resize;
begin
  inherited;
  SetCustomControlHeight;
end;

procedure TAdvCustomFilterPanel.RestoreFilterClick(Sender: TObject);
begin
  if Assigned(FOnRestoreFilter) then
    FOnRestoreFilter(Self);
end;

procedure TAdvCustomFilterPanel.CreateWnd;
begin
  inherited;
  FCustomControl.Width := FScrollBox.ClientWidth;
  FCustomControl.Height := FScrollBox.ClientHeight;
end;

destructor TAdvCustomFilterPanel.Destroy;
begin
  FFilterUI.Free;
  FLabelFont.Free;
  FTitleFont.Free;
  FButtonAddIcon.Free;
  FButtonRemoveIcon.Free;
  FButtonAppearance.Free;
  inherited;
end;

procedure TAdvCustomFilterPanel.SetTitleAlignment(const Value: TAlignment);
begin
  FTitleAlignment := Value;
  FHeader.Alignment := Value;
end;

procedure TAdvCustomFilterPanel.SetTitle(const Value: string);
begin
  FTitle := Value;
  FHeader.Text := Value;
end;

procedure TAdvCustomFilterPanel.SetVersion(const Value: string);
begin
end;

function TAdvCustomFilterPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomFilterPanel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomFilterPanel.Init;
begin
  //
end;

procedure TAdvCustomFilterPanel.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TAdvCustomFilterPanel.SetFCustomControl(const Value: TAdvFilterContainer);
begin
  FCustomControl.Assign(Value);
end;

procedure TAdvCustomFilterPanel.SetFooterColorDirection(const Value: TAdvItemGradientDirection);
begin
  FFooterColorDirection := Value;
  FFooter.Direction := FFooterColorDirection <> gdVertical;
end;

procedure TAdvCustomFilterPanel.SetFooterColorEnd(const Value: Tcolor);
begin
  FFooterColorEnd := Value;
  FFooter.ColorTo := FooterColorEnd;
end;

procedure TAdvCustomFilterPanel.SetFooterColorStart(const Value: TColor);
begin
  FFooterColorStart := Value;
  FFooter.Color := FFooterColorStart;
end;

procedure TAdvCustomFilterPanel.SetHeaderColorDirection(const Value: TAdvItemGradientDirection);
begin
  FHeaderColorDirection := Value;
  FHeader.Direction := HeaderColorDirection <> gdVertical;
end;

procedure TAdvCustomFilterPanel.SetHeaderColorEnd(const Value: TColor);
begin
  FHeaderColorEnd := Value;
  FHeader.ColorTo := FHeaderColorEnd;
end;

procedure TAdvCustomFilterPanel.SetHeaderColorStart(const Value: TColor);
begin
  FHeaderColorStart := Value;
  FHeader.Color := FHeaderColorStart;
end;

procedure TAdvCustomFilterPanel.SetColorDirection(const Value: TAdvItemGradientDirection);
begin
  if FColorDirection <> Value then
  begin
    FColorDirection := Value;
    FCustomControl.Direction := FColorDirection <> gdVertical;
  end;
end;

procedure TAdvCustomFilterPanel.SetColorEnd(const Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    FCustomControl.ColorTo := FColorEnd;
  end;
end;

procedure TAdvCustomFilterPanel.SetColorStart(const Value: TColor);
begin
  if FColorStart <> Value then
  begin
    FColorStart := Value;
    FCustomControl.Color := FColorStart;
  end;
end;

procedure TAdvCustomFilterPanel.SetShowApplyFilter(const Value: Boolean);
begin
  FShowApplyFilter := Value;
  FFooterAddFilterButton.Visible := FShowApplyFilter;
end;

procedure TAdvCustomFilterPanel.SetShowCase(const Value: Boolean);
var
  I: Integer;
  C: TCustomControl;
  Y: Integer;
begin
  FShowCase := Value;

  for Y := 0 to FCustomControl.ControlCount - 1 do
  begin
    if FCustomControl.Controls[Y] is TCustomControl then
    begin
      C := TCustomControl(FCustomControl.Controls[Y]);

      if Assigned(C) then
      begin
        for I := C.ControlCount - 1 downto 0 do
        begin
          if (C.Controls[I].Tag = tagCase) then
          begin
            C.Controls[I].Visible := Value;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.SetShowClearFilter(const Value: Boolean);
begin
  FShowClearFilter := Value;
  FFooterAddClearButton.Visible := FShowClearFilter;
end;

procedure TAdvCustomFilterPanel.SetShowRestoreFilter(const Value: Boolean);
begin
  FShowRestoreFilter := Value;
  FFooterRestoreFilterButton.Visible := FShowRestoreFilter;
end;

procedure TAdvCustomFilterPanel.SetSortColumnNames(const Value: boolean);
begin
  if (FSortColumnNames <> Value) then
  begin
    FSortColumnNames := Value;
    Init;
  end;
end;

procedure TAdvCustomFilterPanel.SetItemBorderColor(const Value: TColor);
var
  I: Integer;
begin
  FItemBorderColor := Value;

  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if FCustomControl.Controls[I] is TAdvFilterGroupBox then
      (FCustomControl.Controls[I] as TAdvFilterGroupBox).BorderColor := FItemBorderColor;
  end;
end;

procedure TAdvCustomFilterPanel.SetItemColorDirection(const Value: TAdvItemGradientDirection);
var
  I: Integer;
  C: TAdvFilterGroupBox;
begin
  FItemColorDirection := Value;

  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if FCustomControl.Controls[I] is TAdvFilterGroupBox then
    begin
      C := TAdvFilterGroupBox(FCustomControl.Controls[I]);
      C.Direction := FItemColorDirection <> gdVertical;
      C.Invalidate;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.SetItemColorEnd(const Value: TColor);
var
  I: Integer;
begin
  FItemColorEnd := Value;

  if Assigned(FCustomControl) then
  begin
    for I := 0 to FCustomControl.ControlCount - 1 do
    begin
      if FCustomControl.Controls[I] is TAdvFilterGroupBox then
        (FCustomControl.Controls[I] as TAdvFilterGroupBox).ColorTo := FItemColorEnd;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.SetItemColorStart(const Value: TColor);
var
  I: Integer;
begin
  FItemColorStart := Value;

  if Assigned(FCustomControl) then
  begin
    for I := 0 to FCustomControl.ControlCount - 1 do
    begin
      if FCustomControl.Controls[I] is TAdvFilterGroupBox then
        (FCustomControl.Controls[I] as TAdvFilterGroupBox).Color := FItemColorStart;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.SetButtonRemoveIcon(const Value: TGDIPPicture);
begin
  FButtonRemoveIcon.Assign(Value);
end;

procedure TAdvCustomFilterPanel.SetButtonAddIcon(const Value: TGDIPPicture);
begin
  FButtonAddIcon.Assign(Value);
end;

procedure TAdvCustomFilterPanel.SetButtonAppearance(const Value: TFilterButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
end;

procedure TAdvCustomFilterPanel.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TAdvCustomFilterPanel.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
end;

procedure TAdvCustomFilterPanel.ApplyLabelFont;
var
  I,Y: Integer;
  C: TCustomControl;
  L: TLabel;
begin
  for I := 0 to FCustomControl.ControlCount - 1 do
  begin
    if (FCustomControl.Controls[I] is TCustomControl)then
    begin
      C := TCustomControl(FCustomControl.Controls[I]);

      for Y := 0 to C.ControlCount - 1 do
      begin
        if C.Controls[Y] is TLabel then
        begin
          L := TLabel(C.Controls[Y]);
          L.Font := LabelFont;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomFilterPanel.SetUI(const Value: TFilterUI);
begin
  FFilterUI.Assign(Value);
end;

procedure TAdvCustomFilterPanel.AddNewRow(position: integer; hasAction: boolean);
var
  clone: TAdvFilterGroupBox;
  ACB, CCB, OCB: TComboBox;
  lbl: TLabel;
  cb: TCheckBox;
  btn: TAdvFilterPanelButton;
  sl: TStringList;
  I: Integer;
  E: TEdit;
begin
  ACB := nil;

  if hasAction then
  begin
    // ACTION COMBOBOX
    ACB := TComboBox.Create(Self);
    ACB.Visible := false;
    ACB.Parent := CustomControl;
    // positions
    ACB.Top := Round((containerY * Position) - 20);
    ACB.Left := Round(actionX);
    ACB.Width:= 60;
    // Items
    ACB.Items.Add(UI.OperatorAnd);
    ACB.Items.Add(UI.OperatorOr);
    ACB.ItemIndex := 0;
    ACB.Tag := tagAction;
    ACB.Style := csDropDownList;
  end;

  // container
  clone := TAdvFilterGroupBox.Create(CustomControl);
  clone.Name := 'container' + IntToStr(Position + 1) + IntToStr(Random(1000));
  clone.Top := Round(5 + ( containerY * (Position) ));
  clone.Left := containerX;
  clone.Height := containerHeight;
  clone.Width := Width -  (2 * containerX);
  clone.Parent := CustomControl;
  clone.Visible := false;
  clone.Color := ItemColorStart;
  clone.ColorTo := ItemColorEnd;
  clone.BorderColor := ItemBorderColor;
  clone.Direction := ItemColorDirection <> gdVertical;
  clone.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];

  // CLONE CONTROLS

  // labels
  // Column
  lbl := TLabel.Create(clone);
  lbl.Left := labelColumnX;
  lbl.Top := labelColumnY;
  lbl.Tag := tagColumn;
  lbl.AutoSize := True;
  lbl.Caption:= UI.ColumnLabel;
  lbl.Parent := clone;
  lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  {$IFDEF DELPHIXE6_LVL}
  lbl.StyledSettings := lbl.StyledSettings - [TStyledSetting.FontColor];
  {$ENDIF}

  // Value
  lbl := TLabel.Create(clone);
  lbl.Left := labelValueX;
  lbl.Top := labelValueY;
  lbl.Tag := tagValue;
  lbl.AutoSize := True;
  lbl.Caption := UI.ValueLabel;
  lbl.Parent := clone;
  lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  {$IFDEF DELPHIXE6_LVL}
  lbl.StyledSettings := lbl.StyledSettings - [TStyledSetting.FontColor];
  {$ENDIF}

  // Filter Operation
  lbl := TLabel.Create(clone);
  lbl.Left := labelOperationX;
  lbl.Top := labelOperationY;
  lbl.Tag := tagOperation;
  lbl.Caption := UI.OperationLabel;
  lbl.Parent := clone;
  lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  lbl.AutoSize := True;
  {$IFDEF DELPHIXE6_LVL}
  lbl.StyledSettings := lbl.StyledSettings - [TStyledSetting.FontColor];
  {$ENDIF}

  // Case
  lbl := TLabel.Create(clone);
  lbl.Left := labelCaseX;
  lbl.Top := labelCaseY;
  lbl.Tag := tagCase;
  lbl.AutoSize := True;
  lbl.Caption := UI.CaseLabel;
  lbl.Parent := clone;
  lbl.Visible := ShowCase;
  {$IFDEF DELPHIXE6_LVL}
  lbl.StyledSettings := lbl.StyledSettings - [TStyledSetting.FontColor];
  {$ENDIF}

  // controls
  // column
  CCB := TComboBox.Create(clone);
  CCB.Left := labelColumnX;
  CCB.Top := labelColumnY + lbl.Height + 5;
  CCB.Width := columnWidth;
  CCB.TabOrder := 0;
  CCB.Parent := clone;
  CCB.Sorted := SortColumnNames;
  if ShowHint then
    CCB.Hint := UI.ColumnHint;
  CCB.OnChange := ColumnChangeHandler;
  sl := GetColumnNames;
  for I := 0 to sl.Count-1 do
  begin
    CCB.Items.AddObject(sl[I], sl.Objects[I]);
  end;
  sl.Free;

  if DefaultColumn < CCB.Items.Count then
  begin
    CCB.ItemIndex := DefaultColumn;
  end;
  CCB.Tag := tagColumn;
  CCB.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  CCB.Style := csDropDownList;
  CCB.TabStop := true;
  CCB.TabOrder := 0;

  // operation
  OCB := TComboBox.Create(clone);
  OCB.Left := labelOperationX;
  OCB.Top := lbl.Top + lbl.Height + 5;
  OCB.Width := operationWidth;
  OCB.Height := 21;
  OCB.TabOrder := 1;
  OCB.Parent := clone;
  OCB.Style := csDropDownList;
  sl := GetFilterOperations;
  OCB.Items.Assign(sl);
  sl.Free;
  OCB.Tag := tagOperation;
  if ShowHint then
    OCB.Hint := UI.OperationHint;
  OCB.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  OCB.TabStop := true;
  OCB.TabOrder := 1;

  if (DefaultColumn <> -1) then
    ColumnChangeHandler(CCB);

  // todo
  E := TEdit.Create(clone);
  E.Left := 310;
  E.top := 28;
  E.Width := 121;
  E.Height := 21;
  E.Parent := clone;
  E.Tag := 3;
  E.Text := UI.ValueTextHint;
  E.TabStop := true;
  E.TabOrder := 2;
  E.Enabled := False;

  // case sensitive
  cb := TCheckBox.Create(clone);
  cb.Top := labelCaseY + lbl.Height + 8;
  cb.Left := labelCaseX + 5;
  cb.Width := caseWidth;
  cb.Height := 13;
  cb.Caption := '';
  cb.TabOrder := 3;
  cb.Parent := clone;
  cb.Tag := tagCase;
  if ShowHint then
    cb.Hint := UI.CaseHint;
  cb.TabStop := true;
  cb.TabOrder := 3;

  // add btn
  btn := TAdvFilterPanelButton.Create(clone);;
  btn.Left := labelCaseX + 40;
  btn.AlignWithMargins := true;
  btn.Top := lbl.Top + lbl.Height + 5;
  btn.Tag := tagButtonAdd;
  btn.Width := 23;
  btn.Height := 22;
  btn.Cursor := crHandPoint;
  btn.OnClick := AddClickHandler;
  btn.Parent := clone;
  btn.Picture.Assign(FButtonAddIcon);
  //tabstop / order

  // remove btn
  btn := TAdvFilterPanelButton.Create(clone);
  btn.Left := labelCaseX + 75;
  btn.Top := lbl.Top + lbl.Height + 5;
  btn.Tag := tagButtonRemove;
  btn.Width := 23;
  btn.Height := 22;
  btn.Cursor := crHandPoint;
  btn.OnClick := RemoveClickHandler;
  btn.Parent := clone;
  btn.Picture.Assign(FButtonRemoveIcon);
  //tabstop / order

  clone.Height := CCB.Top + CCB.Height + 5;
  clone.Margins.Left := 5;
  clone.Margins.Top := 5;
  clone.Margins.Right := 5;

  clone.Visible := true;

  if hasAction and Assigned(ACB) then
    ACB.Visible := true;
end;

procedure TAdvCustomFilterPanel.AddSpinEdit(groupbox: TCustomControl; AText: string);
var
  Spin: TSpinEdit;
begin
  Spin := TSpinEdit.Create(groupbox);
  with Spin do
  begin
    Left := 310;
    top := 28;
    Width := 121;
    Height := 16;
    Parent := groupbox;
    Tag := 3;
    Text := AText;
    TabStop := true;
    TabOrder := 2;
  end;
end;

function TAdvCustomFilterPanel.AddEdit(groupbox: TCustomControl; AColumn: integer; AText: string) : TControl;
var
  ed: TEdit;
begin
  ed := TEdit.Create(groupbox);
  with ed do
  begin
    Left := 310;
    top := 28;
    Width := 121;
    Height := 21;
    Parent := groupbox;
    Tag := 3;
    Text := AText;
    TabStop := true;
    TabOrder := 2;
  end;

  Result := ed;
end;

procedure TAdvCustomFilterPanel.AddFilterClick(Sender: TObject);
begin
  if Assigned(FOnApplyFilter) then
    FOnApplyFilter(Self);
end;

function TAdvCustomFilterPanel.AddBool(groupbox: TCustomControl; AChecked: Boolean = false) : TCheckBox;
var
  ch: TCheckBox;
begin

  ch := TCheckBox.Create(groupbox);
  with ch do
  begin
    Left := 315;
    top := 31;
    Width := 13;
    Height := 13;
    Parent := groupbox;
    Tag := 3;
    TabStop := true;
    TabOrder := 2;
    Checked := AChecked;
  end;

  Result := ch;
end;

procedure TAdvCustomFilterPanel.AddClear(Sender: TObject);
begin
  if Assigned(FOnRemoveFilter) then
    FOnRemoveFilter(Self);
end;

procedure TAdvCustomFilterPanel.AddDate(groupbox: TCustomControl; AText: string);
var
  date: TDateTimePicker;
begin
  date := TDateTimePicker.Create(groupbox);
  with date do
  begin
    Left := 310;
    top := 28;
    Width := 121;
    Height := 21;
    Parent := groupbox;
    Tag := 3;
    TabStop := true;
    TabOrder := 2;
    Date:= StrToDate(AText);
  end;
end;

{ TAdvFilterContainer }

constructor TAdvFilterContainer.Create(AOwner: TComponent);
begin
  inherited;
  FColor := TAdvCustomFilterPanel(AOwner).ColorStart;
  FColorTo := TAdvCustomFilterPanel(AOwner).ColorEnd;
  FDirection := TAdvCustomFilterPanel(AOwner).ColorDirection <> gdVertical;
  FAlignment := taLeftJustify;
end;

procedure TAdvFilterContainer.Paint;
var
  R: TRect;
  DTSTYLE: DWORD;
begin
  inherited;

  DrawGradient(Canvas, Color, ColorTo, 64, Bounds(0, 0, Width, Height), Direction);

  R := ClientRect;
  R.Left := 2;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  DTSTYLE := DT_SINGLELINE or DT_VCENTER or Alignments[Alignment];

  DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DTSTYLE);
end;

procedure TAdvFilterContainer.SetAlignment(const Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterContainer.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterContainer.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterContainer.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterContainer.SetDirection(const Value: Boolean);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    Invalidate;
  end;
end;

{ TAdvFilterGroupBox }

constructor TAdvFilterGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  FColor := TAdvCustomFilterPanel(TScrollBox(AOwner).Parent).ItemColorStart;
  FColorTo := TAdvCustomFilterPanel(TScrollBox(AOwner).Parent).ItemColorEnd;
  FDirection := TAdvCustomFilterPanel(TScrollBox(AOwner).Parent).ItemColorDirection <> gdVertical;
end;

procedure TAdvFilterGroupBox.Paint;
begin
  inherited;

  DrawGradient(Canvas, Color, ColorTo, 64, ClientRect, Direction);

  if BorderColor <> clNone then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;
end;

procedure TAdvFilterGroupBox.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterGroupBox.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterGroupBox.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvFilterGroupBox.SetDirection(const Value: Boolean);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    Invalidate;
  end;
end;

{ TFilterUI }

procedure TFilterUI.Assign(Source: TPersistent);
begin
  if (Source is TFilterUI) then
  begin
    FColumnLabel := (Source as TFilterUI).ColumnLabel;
    FColumnHint := (Source as TFilterUI).ColumnHint;
    FOperationLabel := (Source as TFilterUI).OperationLabel;
    FOperationHint := (Source as TFilterUI).OperationHint;
    FValueLabel := (Source as TFilterUI).ValueLabel;
    FCaseLabel := (Source as TFilterUI).CaseLabel;
    FCaseHint := (Source as TFilterUI).CaseHint;
  end;
end;

constructor TFilterUI.Create(AOwner: TPersistent);
begin
  inherited Create;

  FApplyButton := 'Apply Filter';
  FApplyDialogText := 'Are you sure you want to apply the filter?';
  FCaseHint := 'Is the value case sensitive';
  FCaseLabel := 'Case';
  FClearButton := 'Clear Filter';
  FClearDialogText := 'Are you sure you want to clear the filter?';
  FColumnLabel := 'Column';
  FColumnHint := 'The column from the selected grid';
  FHintFilterAdd := 'Add a new filter';
  FHintFilterRemove := 'Remove this filter';
  FHintApplyFilter := 'Applies the new filter conditions';
  FHintClearFilter := 'Clears the visual filter conditions, but remains the active grid filter';
  FHintRestoreFilter := 'Restores the active grid filter to the filter panel';
  FOperationEqual := 'Equal';
  FOperationNotEqual := 'Not Equal';
  FOperationContains := 'Contains';
  FOperationSmallerThen := 'Smaller Then';
  FOperationLargerThen := 'Larger Then';
  FOperationSmallerOrEqual := 'Smaller Or Equal';
  FOperationLargerOrEqual := 'Larger Or Equal';
  FOperationBeginsWith := 'Begins With';
  FOperationEndsWith := 'Ends With';
  FOperationTrueFalse := 'True/False';
  FOperationLabel := 'Filter Operation';
  FOperationHint := 'Select an operation';
  FOperatorAnd := 'And';
  FOperatorOr := 'Or';
  FRestoreButton := 'Restore Filter';
  FRestoreDialogText := 'Are you sure you want to restore the filter?';
  FValueLabel := 'Value';
  FValueTextHint := 'Select a column first';

end;

procedure TFilterUI.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TFilterUI.SetApplyButton(const Value: string);
begin
  FApplyButton := Value;
  DoChange;
end;

procedure TFilterUI.SetApplyDialogText(const Value: string);
begin
  FApplyDialogText := Value;
  DoChange;
end;

procedure TFilterUI.SetCaseHint(const Value: string);
begin
  FCaseHint := Value;
  DoChange;
end;

procedure TFilterUI.SetCaseLabel(const Value: string);
begin
  if (FCaseLabel <> Value) then
  begin
    FCaseLabel := Value;
    DoChange;
  end;
end;

procedure TFilterUI.SetColumnHint(const Value: string);
begin
  FColumnHint := Value;
  DoChange;
end;

procedure TFilterUI.SetColumnLabel(const Value: string);
begin
  FColumnLabel := Value;
  DoChange;
end;

procedure TFilterUI.SetHintApplyFilter(const Value: string);
begin
  FHintApplyFilter := Value;
  DoChange;
end;

procedure TFilterUI.SetHintClearFilter(const Value: string);
begin
  FHintClearFilter := Value;
  DoChange;
end;

procedure TFilterUI.SetHintFilterAdd(const Value: string);
begin
  FHintFilterAdd := Value;
  DoChange;
end;

procedure TFilterUI.SetHintFilterRemove(const Value: string);
begin
  FHintFilterRemove := Value;
  DoChange;
end;

procedure TFilterUI.SetHintRestoreFilter(const Value: string);
begin
  FHintRestoreFilter := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationBeginsWith(const Value: string);
begin
  FOperationBeginsWith := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationContains(const Value: string);
begin
  FOperationContains := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationEndsWith(const Value: string);
begin
  FOperationEndsWith := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationEqual(const Value: string);
begin
  FOperationEqual := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationHint(const Value: string);
begin
  FOperationHint := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationLabel(const Value: string);
begin
  FOperationLabel := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationLargerOrEqual(const Value: string);
begin
  FOperationLargerOrEqual := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationLargerThen(const Value: string);
begin
  FOperationLargerThen := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationNotEqual(const Value: string);
begin
  FOperationNotEqual := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationSmallerOrEqual(const Value: string);
begin
  FOperationSmallerOrEqual := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationSmallerThen(const Value: string);
begin
  FOperationSmallerThen := Value;
  DoChange;
end;

procedure TFilterUI.SetOperationTrueFalse(const Value: string);
begin
  FOperationTrueFalse := Value;
  DoChange;
end;

procedure TFilterUI.SetOperatorAnd(const Value: string);
begin
  FOperatorAnd := Value;
  DoChange;
end;

procedure TFilterUI.SetOperatorOr(const Value: string);
begin
  FOperatorOr := Value;
  DoChange;
end;

procedure TFilterUI.SetClearButton(const Value: string);
begin
  FClearButton := Value;
  DoChange;
end;

procedure TFilterUI.SetClearDialogText(const Value: string);
begin
  FClearDialogText := Value;
  DoChange;
end;

procedure TFilterUI.SetRestoreButton(const Value: string);
begin
  FRestoreButton := Value;
  DoChange;
end;

procedure TFilterUI.SetRestoreDialogText(const Value: string);
begin
  FRestoreDialogText := Value;
  DoChange;
end;

procedure TFilterUI.SetValueLabel(const Value: string);
begin
  FValueLabel := Value;
  DoChange;
end;

procedure TFilterUI.SetValueTextHint(const Value: string);
begin
  FValueTextHint := Value;
  DoChange;
end;

{ TAdvCustomGridFilterDialog }

constructor TAdvCustomGridFilterDialog.Create(AOwner: TComponent);
begin
  inherited;

  FTitleFont := TFont.Create;
  FTitleFont.Color := clWhite;

  FLabelFont := TFont.Create;

  // defaults
  FColorStart := clWhite;
  FColorEnd := clNone;
  FColorDirection := gdHorizontal;

  FItemBorderColor := clGray;
  FItemColorStart := clSilver;
  FItemColorEnd := clNone;
  FItemColorDirection := gdHorizontal;

  FHeaderColorStart := clGray;
  FHeaderColorEnd := clNone;
  FHeaderColorDirection := gdHorizontal;

  FFooterColorStart := clSilver;
  FFooterColorEnd := clNone;
  FFooterColorDirection := gdHorizontal;

  FShowApplyFilter := true;
  FShowClearFilter := true;
  FShowRestoreFilter := true;
  FShowCase := true;

  FFilterUI := TFilterUI.Create(Self);

  FButtonRemoveIcon := TGDIPPicture.Create;
  FButtonAddIcon := TGDIPPicture.Create;

  FButtonAddIcon.LoadFromResourceName(HInstance, 'ADVCUSTOMFILTERADD');
  FButtonRemoveIcon.LoadFromResourceName(HInstance, 'ADVCUSTOMFILTERREMOVE');
end;

destructor TAdvCustomGridFilterDialog.Destroy;
begin
  FFilterUI.Free;
  FButtonRemoveIcon.Free;
  FButtonAddIcon.Free;
  FTitleFont.Free;
  FLabelFont.Free;
  inherited;
end;

procedure TAdvCustomGridFilterDialog.DoApplyFilter(Sender: TObject);
begin
  if Assigned(OnApplyFilter) then
    OnApplyFilter(Sender);
end;

procedure TAdvCustomGridFilterDialog.DoRemoveFilter(Sender: TObject);
begin
  if Assigned(OnRemoveFilter) then
    OnRemoveFilter(Sender);
end;

procedure TAdvCustomGridFilterDialog.DoRestoreFilter(Sender: TObject);
begin
 if Assigned(OnRestoreFilter) then
   OnRestoreFilter(Sender);
end;

procedure TAdvCustomGridFilterDialog.Execute;
begin
  FForm := CreateFilterForm;
  FForm.FilterPanel.UI.Assign(UI);
  FForm.OnApplyFilter := DoApplyFilter;
  FForm.OnRestoreFilter := DoRestoreFilter;
  FForm.OnRemoveFilter := DoRemoveFilter;
  FForm.OnShow := FormShow;
  try
    FForm.Show;
  finally
  end;
end;

procedure TAdvCustomGridFilterDialog.FormShow(Sender: TObject);
begin
  FForm.Caption := FCaption;
  FForm.FilterPanel.ColorStart := ColorStart;
  FForm.FilterPanel.ColorEnd := ColorEnd;
  FForm.FilterPanel.ColorDirection := ColorDirection;
  FForm.FilterPanel.FooterColorStart := FooterColorStart;
  FForm.FilterPanel.FooterColorEnd := FooterColorEnd;
  FForm.FilterPanel.FooterColorDirection := FooterColorDirection;
  FForm.FilterPanel.HeaderColorStart := HeaderColorStart;
  FForm.FilterPanel.HeaderColorEnd := HeaderColorEnd;
  FForm.FilterPanel.HeaderColorDirection := HeaderColorDirection;
  FForm.FilterPanel.ItemBorderColor := ItemBorderColor;
  FForm.FilterPanel.ItemColorStart := ItemColorStart;
  FForm.FilterPanel.ItemColorEnd := ItemColorEnd;
  FForm.FilterPanel.ItemColorDirection := ItemColorDirection;
  FForm.FilterPanel.LabelFont := LabelFont;
  FForm.FilterPanel.ShowApplyFilter := ShowApplyFilter;
  FForm.FilterPanel.ShowClearFilter := ShowClearFilter;
  FForm.FilterPanel.ShowRestoreFilter := ShowRestoreFilter;
  FForm.FilterPanel.ShowCase := ShowCase;
  FForm.FilterPanel.TitleAlignment := TitleAlignment;
  FForm.FilterPanel.TitleFont := TitleFont;
  FForm.FilterPanel.Title := Title;
  FForm.FilterPanel.UI := UI;
  FForm.FilterPanel.ButtonAddIcon.Assign(ButtonAddIcon);
  FForm.FilterPanel.ButtonRemoveIcon.Assign(ButtonRemoveIcon);
end;

procedure TAdvCustomGridFilterDialog.SetButtonAddIcon(const Value: TGDIPPicture);
begin
  FButtonAddIcon.Assign(Value);
end;

procedure TAdvCustomGridFilterDialog.SetButtonRemoveIcon(const Value: TGDIPPicture);
begin
  FButtonRemoveIcon.Assign(Value);
end;

procedure TAdvCustomGridFilterDialog.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TAdvCustomGridFilterDialog.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
end;

procedure TAdvCustomGridFilterDialog.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TAdvCustomGridFilterDialog.SetUI(const Value: TFilterUI);
begin
  UI.Assign(Value);
end;

{ TAdvCustomFilterForm }

constructor TAdvCustomFilterForm.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited CreateNew(AOwner, Dummy);
  FFilterPanel := CreateFilterPanel(Self);
  FFilterPanel.Align := alClient;
  FFilterPanel.Parent := Self;

  FFilterPanel.OnRestoreFilter := DoRestoreFilter;
  FFilterPanel.OnApplyFilter := DoApplyFilter;
  FFilterPanel.OnRemoveFilter := DoRemoveFilter;

  Width := 600;
  Height := 400;
end;

procedure TAdvCustomFilterForm.CreateWnd;
begin
  inherited;
  FFilterPanel.Parent := Self;
  Position := poScreenCenter;
end;


procedure TAdvCustomFilterForm.DoApplyFilter(Sender: TObject);
begin
  if Assigned(OnApplyFilter) then
    OnApplyFilter(Sender);
end;

procedure TAdvCustomFilterForm.DoRemoveFilter(Sender: TObject);
begin
  if Assigned(OnRemoveFilter) then
    OnRemoveFilter(Sender);
end;

procedure TAdvCustomFilterForm.DoRestoreFilter(Sender: TObject);
begin
  if Assigned(OnRestoreFilter) then
    OnRestoreFilter(Sender);
end;

end.
