{***************************************************************************}
{ TDBAdvSmoothComboBox component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit DBAdvSmoothComboBox;

interface

uses
  Windows, Classes, StdCtrls, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, Math,
  AdvSmoothListBox, AdvSmoothComboBox;

type
  TDBAdvSmoothComboBox = class;

  TComboBoxDataBinding = class(TPersistent)
  private
    FOwner: TDBAdvSmoothComboBox;
    FOnChange: TNotifyEvent;
    FCaptionField: String;
    FGraphicRightField: string;
    FInfoField: string;
    FCheckedField: string;
    FHintField: string;
    FNotesField: string;
    FProgressValueField: string;
    FGraphicLeftField: string;
    FNotesTemplate: string;
    FCaptionTemplate: String;
    procedure SetCaptionField(const Value: String);
    procedure SetCheckedField(const Value: string);
    procedure SetGraphicLeftField(const Value: string);
    procedure SetGraphicRightField(const Value: string);
    procedure SetHintField(const Value: string);
    procedure SetInfoField(const Value: string);
    procedure SetNotesField(const Value: string);
    procedure SetProgressValueField(const Value: string);
    function GetDBComboBox: TDBAdvSmoothComboBox;
    procedure SetCaptionTemplate(const Value: String);
    procedure SetNotesTemplate(const Value: string);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TDBAdvSmoothComboBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DBAdvSmoothComboBox: TDBAdvSmoothComboBox read GetDBComboBox;
  published
    property CaptionField: String read FCaptionField write SetCaptionField; // DataField
    property CaptionTemplate: String read FCaptionTemplate write SetCaptionTemplate; // DataField
    property CheckedField: string read FCheckedField write SetCheckedField;  // must be boolean
    property NotesField: string read FNotesField write SetNotesField;
    property NotesTemplate: string read FNotesTemplate write SetNotesTemplate;
    property InfoField: string read FInfoField write SetInfoField;
    property HintField: string read FHintField write SetHintField;
    property GraphicLeftField: string read FGraphicLeftField write SetGraphicLeftField;  // must be graphic
    property GraphicRightField: string read FGraphicRightField write SetGraphicRightField;  // must be graphic
    property ProgressValueField: string read FProgressValueField write SetProgressValueField;  // must be integer/float
  end;

  TAdvComboBoxDataLink = class(TDataLink)
  private
    FComboBox: TDBAdvSmoothComboBox;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FLockEffects: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AComboBox: TDBAdvSmoothComboBox);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property ComboBox: TDBAdvSmoothComboBox read FComboBox;
  end;

  TDataBindEvent = procedure (Sender: TObject; Item: TAdvSmoothListBoxItem; DBFields: TFields) of object;
  THTMLTemplateDataEvent = procedure(Sender: TObject; ItemIndex: Integer; FieldName: string; var Data: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSmoothComboBox = class(TAdvSmoothComboBox)
  private
    FDataLink: TAdvComboBoxDataLink;
    FDataBinding: TComboBoxDataBinding;
    FPageMode: Boolean;
    FOldTopIndex: Integer;
    FInternalChange: Boolean;
    FInternalSelectItem: Boolean;
    FOnDataBind: TDataBindEvent;
    FOnGetNotesTemplateData: THTMLTemplateDataEvent;
    FOnGetCaptionTemplateData: THTMLTemplateDataEvent;
    procedure ActiveChange(Value: Boolean);
    procedure EditingChanged;
    procedure DataScroll(Distance: Integer);
    procedure RecordChanged(Field: TField);
    procedure DataChange;
    procedure UpdateData;
    procedure UpdateActiveItem;
    procedure UpdateTopIndex;
    procedure OnDataBindingChanged(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetDataBinding(const Value: TComboBoxDataBinding);
    procedure SetPageMode(const Value: Boolean);
    function GetSelectedIndex: Integer;
    function HTMLDBReplace(s: string; Dataset: Tdataset; ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
  protected
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitPreview; override;
    procedure DoItemText(Sender: TObject; itemindex: integer; var itemcaption: String; var iteminfo: String; var itemnotes: String); override;
    procedure DoItemGraphics(Sender: TObject; itemindex: integer); override;
    procedure DoScroll(Sender: TObject; CurrentPosition, EndPosition: Double); override;
    procedure DoSmoothScroll(CurrentPosition, EndPosition: Double); override;
    procedure DoLookup(DispItem: TAdvSmoothListBoxDisplayListItem); override;
    procedure DoBoolPropertyChange(Item: TAdvSmoothListBoxItem; PropID: Integer; var Value: Boolean); override;
    procedure SetSelectionMode(const Value: TAdvSmoothListBoxSelectionMode); override;
    function DoItemCheckToggle(Item: TAdvSmoothListBoxItem; GraphicLeft: Boolean; var Checked: Boolean): Boolean; override;
    procedure DoInternalScroll; override;
    function LookupBarVisible: Boolean; override;
    procedure DoItemClick(Sender: TObject; itemindex: integer); override;
    procedure SetInternalSelectedItemIndex(Value: Integer); override;
    procedure DoSelectItem(NewItemIndex: Integer); override;
    function CheckDataSet: Boolean;
    function GetRecordCount: Integer;
    procedure UpdateItemCount;
    function CreateListBox: TAdvSmoothListBox; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure LoadFromDataSet;
    procedure Reload;
    procedure DropDown; override;
  published
    property DataBinding: TComboBoxDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property PageMode: Boolean read FPageMode write SetPageMode default true;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True; // Can be shifted to parent class
    property OnDataBind: TDataBindEvent read FOnDataBind write FOnDataBind;
    property OnGetCaptionTemplateData: THTMLTemplateDataEvent read FOnGetCaptionTemplateData write FOnGetCaptionTemplateData;
    property OnGetNotesTemplateData: THTMLTemplateDataEvent read FOnGetNotesTemplateData write FOnGetNotesTemplateData;
  end;


implementation

uses
  VDBConsts, Consts, DBAdvSmoothListBox;

type
   TProAdvSmoothListBoxItem = class(TAdvSmoothListBoxItem);

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//------------------------------------------------------------------------------

{ TAdvComboBoxDataLink }

constructor TAdvComboBoxDataLink.Create(AComboBox: TDBAdvSmoothComboBox);
begin
  inherited Create;
  FComboBox := AComboBox;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

destructor TAdvComboBoxDataLink.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.ActiveChanged;
begin
{$IFDEF DELPHI6_LVL}
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        raise exception.Create('UniDirectional DataSet');
{$ENDIF}
  if not FLockEffects then
    FComboBox.ActiveChange(Active);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.Modified;
begin
  FModified := True;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.DataSetChanged;
begin
  if not FLockEffects then
    FComboBox.DataChange;
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.DataSetScrolled(Distance: Integer);
begin
  if not FLockEffects then
    FComboBox.DataScroll(Distance);
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.EditingChanged;
begin
  if not FLockEffects then
    FComboBox.EditingChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.RecordChanged(Field: TField);
begin
  if not FLockEffects then
    FComboBox.RecordChanged(Field);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified and not FLockEffects then
      FComboBox.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBoxDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

//------------------------------------------------------------------------------

{ TDBAdvSmoothComboBox }

constructor TDBAdvSmoothComboBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TAdvComboBoxDataLink.Create(Self);
  FDataLink.FComboBox := Self;

  FDataBinding := TComboBoxDataBinding.Create(Self);
  FDataBinding.OnChange := OnDataBindingChanged;
  ReadOnly := True;
  SetDisplayItemValues := True;
  FPageMode := True;
  FOldTopIndex := -1;
  SelectionMode := sPersistSelection;  // must be persist
  SkipKeyScrolling := True;
end;

//------------------------------------------------------------------------------

destructor TDBAdvSmoothComboBox.Destroy;
begin
  FDataBinding.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.CreateListBox: TAdvSmoothListBox;
begin
  Result := TDBAdvSmoothListBox.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FOldTopIndex := GetTopIndex;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoScroll(Sender: TObject; CurrentPosition,
  EndPosition: Double);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoLookup(DispItem: TAdvSmoothListBoxDisplayListItem);
var
  i: Integer;
begin
  inherited;

  if CheckDataSet and PageMode then
  begin
    i := GetBottomIndex - 1;  // 1 was extra added in GetBottomIndex
    if (i >= 0) and (i < Items.Count) then
      Items.SelectedItem := Items[i];
    i := GetTopIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
    Items.SelectedItem := DispItem.DisplayItem;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoInternalScroll;
var
  i: Integer;
begin
  inherited;

  if CheckDataSet and PageMode then
  begin
    i := GetBottomIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
    i := GetTopIndex;
    if (i >= 0) then
      Items.SelectedItem := Items[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoSmoothScroll(CurrentPosition, EndPosition: Double);
var
  i: Integer;
  DirDown: Boolean;
begin
  inherited;

  if CheckDataSet and PageMode and not FInternalSelectItem then
  begin
    DirDown := CurrentPosition < EndPosition;
    //OutputDebugString(PChar('Dir: ' + BoolToStr(DirDown) ));

    if (CurrentPosition <> EndPosition) then
    begin
      if DirDown then
      begin
        i := GetBottomIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end
      else
      begin
        i := GetTopIndex;
        if (i >= 0) and (i < Items.Count) then
          Items.SelectedItem := Items[i];
      end;
    end;
    FOldTopIndex := GetTopIndex;
    //OutputDebugString(PChar('Top Index === ' + Inttostr(GetTopIndex) + ' Moved =' + inttostr(i)));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DropDown;
begin
  if PageMode then
  begin
    TDBAdvSmoothListBox(ListBox).DataBinding.CaptionField := DataBinding.CaptionField;
    TDBAdvSmoothListBox(ListBox).DataBinding.CaptionTemplate := DataBinding.CaptionTemplate;
    TDBAdvSmoothListBox(ListBox).DataBinding.CheckedField := DataBinding.CheckedField;
    TDBAdvSmoothListBox(ListBox).DataBinding.InfoField := DataBinding.InfoField;
    TDBAdvSmoothListBox(ListBox).DataBinding.NotesField := DataBinding.NotesField;
    TDBAdvSmoothListBox(ListBox).DataBinding.NotesTemplate := DataBinding.NotesTemplate;
    TDBAdvSmoothListBox(ListBox).DataBinding.HintField := DataBinding.HintField;
    TDBAdvSmoothListBox(ListBox).DataBinding.GraphicLeftField := DataBinding.GraphicLeftField;
    TDBAdvSmoothListBox(ListBox).DataBinding.GraphicRightField := DataBinding.GraphicRightField;
    TDBAdvSmoothListBox(ListBox).DataBinding.ProgressValueField := DataBinding.ProgressValueField;
    TDBAdvSmoothListBox(ListBox).DataSource := DataSource;
    TDBAdvSmoothListBox(ListBox).ReadOnly := ReadOnly;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.UpdateTopIndex;
var
  i, SI: Integer;
begin
  if not CheckDataSet or not PageMode then
    Exit;

  i := GetTopIndex;
  SI := GetSelectedIndex;
  if (SI >= 0) and (SI - i <> FDataLink.ActiveRecord) then
  begin
    SetTopIndex(i + ((SI - i) - FDataLink.ActiveRecord));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetInternalSelectedItemIndex(Value: Integer);
var
  OldV: Integer;
begin
  OldV := FSelectedItemIndex;
  inherited;

  if (OldV <> Value) then
    DoSelectItem(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoSelectItem(NewItemIndex: Integer);
var
  SI: Integer;
begin
  inherited;

  FSelectedItemIndex := NewItemIndex;
  
  if not CheckDataSet or not PageMode then
    Exit;

  if FInternalChange then
    Exit;

  SI := NewItemIndex + 1;

  if (SI > 0) then
  begin
    FInternalChange := True;
    FDataLink.DataSet.MoveBy(SI - FDataLink.DataSet.RecNo);
    FInternalChange := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DataScroll(Distance: Integer);
var
  RN: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) then
  begin
    FInternalChange := True;
    try
      Items.SelectedItem := Items[RN];
      UpdateTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.UpdateActiveItem;
var
  RN, SI: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  SI := GetSelectedIndex;
  //FSelectedItemIndex := SI;
  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) and (SI <> RN) then
  begin
    FInternalChange := True;
    try
      Items.SelectedItem := Items[RN];
      UpdateTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoItemGraphics(Sender: TObject;
  itemindex: integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoBoolPropertyChange(Item: TAdvSmoothListBoxItem;
  PropID: Integer; var Value: Boolean);
begin
  inherited;
  case PropID of
    1, 2: Value := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoItemText(Sender: TObject; itemindex: integer;
  var itemcaption, iteminfo, itemnotes: String);
var
  aField: TField;
  blobf: TBlobField;
  s: TStream;
  P: TNotifyEvent;
begin
  if PageMode and CheckDataSet then
  begin

    if not Assigned(Items.SelectedItem) then
      Exit;
    try
      if (DataBinding.CaptionTemplate <> '') then
      begin
        itemCaption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, itemindex, FOnGetCaptionTemplateData);
      end
      else if (DataBinding.CaptionField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
        if Assigned(aField) then
          itemCaption := aField.Text;
      end;

      if (DataBinding.InfoField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.InfoField);
        if Assigned(aField) then
          itemInfo := aField.Text;
      end;

      if (DataBinding.NotesTemplate <> '') then
        itemNotes := HTMLDBReplace(DataBinding.NotesTemplate, FDataLink.DataSet, itemindex, FOnGetNotesTemplateData)
      else if (DataBinding.NotesField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.NotesField);
        if Assigned(aField) then
          itemNotes := aField.Text;
      end;

      if (DataBinding.CheckedField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
        if Assigned(aField) and (aField.DataType = ftBoolean) then
          TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalChecked(aField.AsBoolean);
      end;

      if (DataBinding.HintField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
        if Assigned(aField) then
          TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalHint(aField.Text);
      end;

      if (DataBinding.ProgressValueField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.ProgressValueField);
        if Assigned(aField) then
        begin
          if (aField.DataType = ftInteger) then
            TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalProgressValue(aField.AsInteger)
          else if (aField.DataType = ftFloat) then
            TProAdvSmoothListBoxItem(Items[itemindex]).SetInternalProgressValue(aField.AsFloat);
        end;
      end;

      if (DataBinding.GraphicLeftField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicLeftField);
        if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
        begin
          blobf := aField as TBlobField;
          s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
          P := Items[itemindex].GraphicLeft.OnChange;
          try
            s.Position := 0;
            Items[itemindex].GraphicLeft.OnChange := nil;
            Items[itemindex].GraphicLeft.LoadFromStream(s);
            //Items[itemindex].GraphicLeftType := gtImage;   // introducing CalcRect issue
          finally
            Items[itemindex].GraphicLeft.OnChange := p;
            s.Free;
          end;
        end;
      end;

      if (DataBinding.GraphicRightField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicRightField);
        if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
        begin
          blobf := aField as TBlobField;
          s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
          P := Items[itemindex].GraphicRight.OnChange;
          try
            s.Position := 0;
            Items[itemindex].GraphicRight.OnChange := nil;
            Items[itemindex].GraphicRight.LoadFromStream(s);
            //Items[itemindex].GraphicRightType := gtImage;   // introducing CalcRect issue
          finally
            Items[itemindex].GraphicRight.OnChange := P;
            s.Free;
          end;
        end;
      end;

    finally
      //FDataLink.ActiveRecord := OldAR;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DataChange;
begin
  if not Assigned(FDataLink.DataSet) or not PageMode then
    Exit;

  UpdateItemCount;
  UpdateActiveItem;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.ActiveChange(Value: Boolean);
var
  rc: Integer;
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if FDataLink.DataSet.Active then
      begin
        Items.Clear;
        if not FDataLink.DataSet.IsSequenced or not PageMode then
        begin
          PageMode := False;      // PageMode = true is not allowed with NonSequenced DataSets
          LoadFromDataSet;
        end
        else
        begin
          rc := GetRecordCount;
          ItemCount := rc;
          //vic := GetVisibleItemCount;
          FDataLink.BufferCount := 1; //vic + 1;

          UpdateActiveItem;
        end;
      end
      else  // Active = False
      begin
        Items.Clear;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.DoItemClick(Sender: TObject; itemindex: integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetSelectionMode(
  const Value: TAdvSmoothListBoxSelectionMode);
begin
  if (Value <> sAutoDeselect) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.UpdateData;
begin
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.DoItemCheckToggle(Item: TAdvSmoothListBoxItem;
  GraphicLeft: Boolean; var Checked: Boolean): Boolean;
var
  aField: TField;
begin
  inherited DoItemCheckToggle(Item, GraphicLeft, Checked);
  Result := False;
  if not (csDesigning in ComponentState) and CheckDataSet and PageMode and Assigned(Item)
    and not Readonly and(DataBinding.CheckedField <> '') then
  begin
    aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
    if Assigned(aField) and (aField.DataType = ftBoolean) then
    begin
      if (GetSelectedIndex <> Item.Index) then
        SelectedItemIndex := Item.Index;
      if FDataLink.Edit then
      begin
        aField.AsBoolean := Checked;
        FDataLink.Modified;
        Result := True;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.RecordChanged(Field: TField);
begin
  if Assigned(Field) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetPageMode(const Value: Boolean);
begin
  if (FPageMode <> Value) then
  begin
    FPageMode := Value;
    if CheckDataSet then
      ActiveChange(FDataLink.DataSet.Active);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.LoadFromDataSet;
var
  aField: TField;
  blobf: TBlobField;
  s: TStream;
  P: TNotifyEvent;
  aItem: TAdvSmoothListBoxItem;
  OldSI: Integer;
begin
  if CheckDataSet {and not PageMode} then
  begin
    FDataLink.DataSet.DisableControls;
    OldSI := FSelectedItemIndex;
    try
      Items.BeginUpdate;
      Items.Clear;
      FDataLink.DataSet.First;
      while not FDataLink.DataSet.Eof do
      begin
        aItem := Items.Add;
        with aItem do
        begin
          if (DataBinding.CaptionTemplate <> '') then
            Caption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, aItem.Index, FOnGetCaptionTemplateData)
          else if (DataBinding.CaptionField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
            if Assigned(aField) then
              Caption := aField.Text;
          end;

          if (DataBinding.InfoField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.InfoField);
            if Assigned(aField) then
              Info := aField.Text;
          end;

          if (DataBinding.NotesTemplate <> '') then
            Notes := HTMLDBReplace(DataBinding.NotesTemplate, FDataLink.DataSet, aItem.Index, FOnGetNotesTemplateData)
          else if (DataBinding.NotesField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.NotesField);
            if Assigned(aField) then
              Notes := aField.Text;
          end;

          if (DataBinding.CheckedField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.CheckedField);
            if Assigned(aField) and (aField.DataType = ftBoolean) then
              Checked := aField.AsBoolean;
          end;

          if (DataBinding.HintField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
            if Assigned(aField) then
              Hint := aField.Text;
          end;

          if (DataBinding.ProgressValueField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.ProgressValueField);
            if Assigned(aField) then
            begin
              if (aField.DataType = ftInteger) then
                ProgressValue := aField.AsInteger
              else if (aField.DataType = ftFloat) then
                ProgressValue := aField.AsFloat;
            end;
          end;

          if (DataBinding.GraphicLeftField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicLeftField);
            if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
            begin
              blobf := aField as TBlobField;
              s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
              P := GraphicLeft.OnChange;
              GraphicLeft.OnChange := nil;
              try
                s.Position := 0;
                GraphicLeft.LoadFromStream(s);
                GraphicLeftType := gtImage;
              finally
                GraphicLeft.OnChange := P;
                s.Free;
              end;
            end;
          end;

          if (DataBinding.GraphicRightField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.GraphicRightField);
            if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
            begin
              blobf := aField as TBlobField;
              s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
              P := GraphicRight.OnChange;
              GraphicRight.OnChange := nil;
              try
                s.Position := 0;
                GraphicRight.LoadFromStream(s);
                GraphicRightType := gtImage;
              finally
                GraphicRight.OnChange := P;
                s.Free;
              end;
            end;
          end;

        end;

        if Assigned(OnDataBind) then
          OnDataBind(Self, aItem, FDataLink.DataSet.Fields);
        FDataLink.DataSet.Next;
      end;
    finally
      Items.EndUpdate;
      FDataLink.DataSet.EnableControls;
      if Items.Count > 0 then
        if (OldSI >= 0) and (OldSI < Items.Count) then
          FSelectedItemIndex := OldSI
        else
          FSelectedItemIndex := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.LookupBarVisible: Boolean;
begin
  Result := inherited LookupBarVisible and not PageMode;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.Reload;
begin
  if not PageMode then
    LoadFromDataSet;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.EditingChanged;
begin
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.GetDataSource: TDataSource;
begin
  if not (csDestroying in ComponentState) and Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.GetRecordCount: Integer;
begin
  Result := 0;
  if CheckDataSet then
    Result := FDataLink.DataSet.RecordCount;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.GetSelectedIndex: Integer;
begin
  Result := -1;
  if Assigned(Items.SelectedItem) then
    Result := Items.SelectedItem.Index;
end;

function TDBAdvSmoothComboBox.HTMLDBReplace(s: string; Dataset: Tdataset;
  ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
begin
  beforetag := '';
  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        dbfld := DataSet.FieldByName(fld).DisplayText;
        if Assigned(DataEvent) then
          DataEvent(self, ItemIndex, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.InitPreview;
begin
  Items.Clear;
  Footer.Caption := 'Footer';
  Footer.Font.Size := 10;
  Footer.Font.Color := clWhite;

  Header.Caption := 'Header';
  Header.Font.Size := 10;
  Header.Font.Color := clWhite;

  //--- No default items creation
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  SetDirectSelectedItemIndex(FSelectedItemIndex);
  inherited KeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.OnDataBindingChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetDataBinding(const Value: TComboBoxDataBinding);
begin
  FDataBinding.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothComboBox.UpdateItemCount;
begin
  if CheckDataSet and PageMode then
  begin
    ItemCount := GetRecordCount;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

{ TComboBoxDataBinding }

procedure TComboBoxDataBinding.Assign(Source: TPersistent);
begin
  if (Source is TComboBoxDataBinding) then
  begin
    CaptionField := (Source as TComboBoxDataBinding).CaptionField;
    CaptionTemplate := (Source as TComboBoxDataBinding).CaptionTemplate;
    InfoField := (Source as TComboBoxDataBinding).InfoField;
    NotesField := (Source as TComboBoxDataBinding).NotesField;
    NotesTemplate := (Source as TComboBoxDataBinding).NotesTemplate;
    GraphicLeftField := (Source as TComboBoxDataBinding).GraphicLeftField;
    GraphicRightField := (Source as TComboBoxDataBinding).GraphicRightField;
    CheckedField := (Source as TComboBoxDataBinding).CheckedField;
    HintField := (Source as TComboBoxDataBinding).HintField;
    ProgressValueField := (Source as TComboBoxDataBinding).ProgressValueField;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TComboBoxDataBinding.Create(AOwner: TDBAdvSmoothComboBox);
begin
  inherited Create;
  FOwner := AOwner;
  FCaptionField := '';
  FCaptionTemplate := '';
  FInfoField := '';
  FNotesField := '';
  FNotesTemplate := '';
  FGraphicLeftField := '';
  FGraphicRightField := '';
  FCheckedField := '';
  FHintField := '';
  FProgressValueField := '';
end;

//------------------------------------------------------------------------------

destructor TComboBoxDataBinding.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TComboBoxDataBinding.GetDBComboBox: TDBAdvSmoothComboBox;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetCaptionField(const Value: String);
begin
  if (FCaptionField <> Value) then
  begin
    FCaptionField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetCaptionTemplate(const Value: String);
begin
  if (FCaptionTemplate <> Value) then
  begin
    FCaptionTemplate := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetCheckedField(const Value: string);
begin
  if (FCheckedField <> Value) then
  begin
    FCheckedField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetGraphicLeftField(const Value: string);
begin
  if (FGraphicLeftField <> Value) then
  begin
    FGraphicLeftField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetGraphicRightField(const Value: string);
begin
  if (FGraphicRightField <> Value) then
  begin
    FGraphicRightField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetHintField(const Value: string);
begin
  if (FHintField <> Value) then
  begin
    FHintField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetInfoField(const Value: string);
begin
  if (FInfoField <> Value) then
  begin
    FInfoField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetNotesField(const Value: string);
begin
  if (FNotesField <> Value) then
  begin
    FNotesField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetNotesTemplate(const Value: string);
begin
  if (FNotesTemplate <> Value) then
  begin
    FNotesTemplate := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TComboBoxDataBinding.SetProgressValueField(const Value: string);
begin
  if (FProgressValueField <> Value) then
  begin
    FProgressValueField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

end.
