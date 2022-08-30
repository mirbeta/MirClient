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
unit cxPropertiesConverters;

{$I cxVer.inc}

interface

uses
  cxCustomConverter, cxBlobEdit, StdCtrls, cxConverterUtils, cxImage, cxButtonEdit,
  cxEdit, Classes, Graphics, cxCalc, cxCheckBox, cxDropDownEdit, cxCurrencyEdit,
  cxCalendar, cxDBLookupComboBox, cxHyperLinkEdit, cxImageComboBox, Controls, SysUtils,
  cxMaskEdit, cxMemo, cxMRUEdit, cxSpinEdit, cxTextEdit, cxTimeEdit;

type
  { TcxCustomPropertiesConverter }
  TcxCustomPropertiesConverter = class(TcxCustomConverter)
  protected
    function ConvertPictureTransparency(const AValue: string): TcxImageTransparency;
    function ConvertVertAlignment(const AVertAlignment: string): TcxEditVertAlignment;
  end;

  { TcxBlobEditPropertiesConverter }
  TcxBlobEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function ConvertBlobEditKind(const AKind: string): TcxBlobEditKind;
    function ConvertBlobPaintStyle(const AStyle: string): TcxBlobPaintStyle;
    function GetDestination: TcxBlobEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxBlobEditProperties read GetDestination;
  end;

  { TcxButtonEditPropertiesConverter }
  TcxButtonEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function ConvertButtonKind(const AButtonKind: string): TcxEditButtonKind;
    function GetDestination: TcxButtonEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxButtonEditProperties read GetDestination;
  end;

  { TcxCalcEditPropertiesConverter }
  TcxCalcEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxCalcEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxCalcEditProperties read GetDestination;
  end;

  { TcxCheckBoxPropertiesConverter }
  TcxCheckBoxPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function ConvertNullFieldStyle(const AValue: string): TcxCheckBoxNullValueShowingStyle;
    function GetDestination: TcxCheckBoxProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxCheckBoxProperties read GetDestination;
  end;

  { TcxComboBoxPropertiesConverter }
  TcxComboBoxPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxComboBoxProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxComboBoxProperties read GetDestination;
  end;

  { TcxCurrencyEditPropertiesConverter }
  TcxCurrencyEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxCurrencyEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxCurrencyEditProperties read GetDestination;
  end;

  { TcxDateEditPropertiesConverter }
  TcxDateEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxDateEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxDateEditProperties read GetDestination;
  end;

  { TcxHyperLinkEditPropertiesConverter }
  TcxHyperLinkEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxHyperLinkEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxHyperLinkEditProperties read GetDestination;
  end;

  { TcxImagePropertiesConverter }
  TcxImagePropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxImageProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxImageProperties read GetDestination;
  end;

  { TcxImageComboBoxPropertiesConverter }
  TcxImageComboBoxPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxImageComboBoxProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxImageComboBoxProperties read GetDestination;
  end;

  { TcxLookupComboBoxPropertiesConverter }
  TcxLookupComboBoxPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxLookupComboBoxProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxLookupComboBoxProperties read GetDestination;
  end;

  { TcxMaskEditPropertiesConverter }
  TcxMaskEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxMaskEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxMaskEditProperties read GetDestination;
  end;

  { TcxMemoPropertiesConverter }
  TcxMemoPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxMemoProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxMemoProperties read GetDestination;
  end;

  { TcxMRUEditPropertiesConverter }
  TcxMRUEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxMRUEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxMRUEditProperties read GetDestination;
  end;

  { TcxPopupEditPropertiesConverter }
  TcxPopupEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxPopupEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxPopupEditProperties read GetDestination;
  end;

  { TcxSpinEditPropertiesConverter }
  TcxSpinEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxSpinEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxSpinEditProperties read GetDestination;
  end;

  { TcxTextEditPropertiesConverter }
  TcxTextEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function GetDestination: TcxTextEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxTextEditProperties read GetDestination;
  end;

  { TcxTimeEditPropertiesConverter }
  TcxTimeEditPropertiesConverter = class(TcxCustomPropertiesConverter)
  private
    function ConvertTimeFormat(const AFormat: string): TcxTimeEditTimeFormat;
    function GetDestination: TcxTimeEditProperties;
  protected
    procedure DoImport; override;
  public
    property Destination: TcxTimeEditProperties read GetDestination;
  end;

implementation

uses
  ImgList;

type
  TcxButtonEditPropertiesAccess = class(TcxButtonEditProperties);

{ TcxCustomPropertiesConverter }

function TcxCustomPropertiesConverter.ConvertPictureTransparency(
  const AValue: string): TcxImageTransparency;
begin
  if AValue = 'gtDefault' then
    Result := gtDefault
  else if AValue = 'gtOpaque' then
    Result := gtOpaque
  else if AValue = 'gtTransparent' then
    Result := gtTransparent
  else
    Result := gtDefault;
end;

function TcxCustomPropertiesConverter.ConvertVertAlignment(const AVertAlignment: string): TcxEditVertAlignment;
begin
  if AVertAlignment = 'tlCenter' then
    Result := taVCenter
  else if AVertAlignment = 'tlTop' then
    Result := taTopJustify
  else if AVertAlignment = 'tlBottom' then
    Result := taBottomJustify
  else
    Result := taVCenter;
end;

{ TcxBlobEditPropertiesConverter }

procedure TcxBlobEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    AlwaysSaveData := GetBooleanProperty(Source, 'AlwaysSaveText', AlwaysSaveData);
    BlobEditKind := ConvertBlobEditKind(GetEnumProperty(Source, 'BlobKind'));
    BlobPaintStyle := ConvertBlobPaintStyle(GetEnumProperty(Source, 'BlobPaintStyle'));
    MemoCharCase := ConvertCharCase(GetEnumProperty(Source, 'MemoCharCase'));
    MemoMaxLength := GetIntegerProperty(Source, 'MemoMaxLength', MemoMaxLength);
    MemoOEMConvert := GetBooleanProperty(Source, 'MemoOEMConvert', MemoOEMConvert);
    MemoScrollBars := ConvertScrollStyle(GetEnumProperty(Source, 'MemoScrollBars'));
    MemoWantReturns := GetBooleanProperty(Source, 'MemoWantReturns', MemoWantReturns);
    MemoWantTabs := GetBooleanProperty(Source, 'MemoWantTabs', MemoWantTabs);
    MemoWordWrap := GetBooleanProperty(Source, 'MemoWordWrap', MemoWordWrap);
    PictureAutoSize := GetBooleanProperty(Source, 'PictureAutoSize', PictureAutoSize);
    PictureFilter := GetStringProperty(Source, 'PictureFilter', PictureFilter);
    PictureTransparency := ConvertPictureTransparency(GetEnumProperty(Source, 'PictureTransparency'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    ShowExPopupItems := GetBooleanProperty(Source, 'ShowExPopupItems', ShowExPopupItems);
    ShowPicturePopup := GetBooleanProperty(Source, 'ShowPicturePopup', ShowPicturePopup);
  end;
  inherited DoImport;
end;

function TcxBlobEditPropertiesConverter.ConvertBlobEditKind(
  const AKind: string): TcxBlobEditKind;
begin
  if AKind = 'bkAuto' then
    Result := bekAuto
  else if AKind = 'bkBlob' then
    Result := bekBlob
  else if AKind = 'bkMemo' then
    Result := bekMemo
  else if AKind = 'bkOle' then
    Result := bekOle
  else if AKind = 'bkPict' then
    Result := bekPict
  else
    Result := bekAuto;
end;

function TcxBlobEditPropertiesConverter.ConvertBlobPaintStyle(
  const AStyle: string): TcxBlobPaintStyle;
begin
  if AStyle = 'bpsDefault' then
    Result := bpsDefault
  else if AStyle = 'bpsIcon' then
    Result := bpsIcon
  else if AStyle = 'bpsText' then
    Result := bpsText
  else
    Result := bpsDefault;
end;

function TcxBlobEditPropertiesConverter.GetDestination: TcxBlobEditProperties;
begin
  Result := inherited Destination as TcxBlobEditProperties;
end;

{ TcxButtonEditPropertiesConverter }

procedure TcxButtonEditPropertiesConverter.DoImport;
var
  AButtons: TObject;
  AButton: TcxEditButton;
  I: Integer;
begin
  with TcxButtonEditPropertiesAccess(Destination) do
    HideCursor := GetBooleanProperty(Source, 'HideEditCursor', HideCursor);
  with Destination do
  begin
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    ClickKey := GetIntegerProperty(Source, 'ClickKey', ClickKey);
    AButtons := GetClassProperty(Source, 'Buttons');
    if AButtons <> nil then
    begin
      if AButtons is TCollection then
      with TCollection(AButtons) do
      begin
        Buttons.Clear;
        for I := 0 to Count - 1 do
        begin
          AButton := Buttons.Add;
          AButton.Default := GetBooleanProperty(Items[I], 'Default', AButton.Default);
          AButton.Glyph.Assign(GetClassProperty(Items[I], 'Glyph') as TBitmap);
          AButton.Kind := ConvertButtonKind(GetEnumProperty(Items[I], 'Kind'));
          AButton.LeftAlignment := GetBooleanProperty(Items[I], 'LeftAlignment', AButton.LeftAlignment);
          AButton.Visible := GetBooleanProperty(Items[I], 'Visible', AButton.Visible);
          AButton.Width := GetIntegerProperty(Items[I], 'Width', AButton.Width);
        end;
      end;
    end;
  end;
  inherited DoImport;
end;

function TcxButtonEditPropertiesConverter.ConvertButtonKind(const AButtonKind: string): TcxEditButtonKind;
begin
  if AButtonKind = 'bkDown' then
    Result := bkDown
  else if AButtonKind = 'bkEllipsis' then
    Result := bkEllipsis
  else if AButtonKind = 'bkGlyph' then
    Result := bkGlyph
  else
    Result := bkDown;
end;

function TcxButtonEditPropertiesConverter.GetDestination: TcxButtonEditProperties;
begin
  Result := inherited Destination as TcxButtonEditProperties;
end;

{ TcxCalcEditPropertiesConverter }

procedure TcxCalcEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    BeepOnError := GetBooleanProperty(Source, 'BeepOnError', BeepOnError);
    ButtonGlyph.Assign(GetClassProperty(Source, 'ButtonGlyph') as TBitmap);
    Precision := GetIntegerProperty(Source, 'Precision', Precision);
    QuickClose := GetBooleanProperty(Source, 'QuickClose', QuickClose);
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
  end;
  inherited DoImport;
end;

function TcxCalcEditPropertiesConverter.GetDestination: TcxCalcEditProperties;
begin
  Result := inherited Destination as TcxCalcEditProperties;
end;

{ TcxCheckBoxPropertiesConverter }

procedure TcxCheckBoxPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    AllowGrayed := GetBooleanProperty(Source, 'AllowGrayed', AllowGrayed);
    Glyph.Assign(GetClassProperty(Source, 'Glyph') as TBitmap);
    GlyphCount := GetIntegerProperty(Source, 'GlyphCount', GlyphCount);
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    ValueChecked := GetStringProperty(Source, 'ValueChecked', '');
    ValueGrayed := GetStringProperty(Source, 'ValueGrayed', '');
    ValueUnchecked := GetStringProperty(Source, 'ValueUnchecked', '');
    DisplayChecked := GetStringProperty(Source, 'DisplayChecked', DisplayChecked);
    DisplayGrayed := GetStringProperty(Source, 'DisplayNull', DisplayGrayed);
    DisplayUnchecked := GetStringProperty(Source, 'DisplayUnchecked', DisplayUnchecked);
    NullStyle := ConvertNullFieldStyle(GetEnumProperty(Source, 'ShowNullFieldStyle'));
  end;
  inherited DoImport;
end;

function TcxCheckBoxPropertiesConverter.ConvertNullFieldStyle(const AValue: string): TcxCheckBoxNullValueShowingStyle;
begin
  if AValue = 'nsGrayedChecked' then
    Result := nssGrayedChecked
  else if AValue = 'nsInactive' then
    Result := nssInactive
  else
    Result := nssUnchecked
end;

function TcxCheckBoxPropertiesConverter.GetDestination: TcxCheckBoxProperties;
begin
  Result := inherited Destination as TcxCheckBoxProperties;
end;

{ TcxComboBoxPropertiesConverter }

procedure TcxComboBoxPropertiesConverter.DoImport;
var
  AObject: TObject;
  I: Integer;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ButtonGlyph.Assign(GetClassProperty(Source, 'ButtonGlyph') as TBitmap);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    DropDownRows := GetIntegerProperty(Source, 'DropDownRows', DropDownRows);
    ImmediateDropDown := GetBooleanProperty(Source, 'ImmediateDropDown', ImmediateDropDown);
    AObject := GetClassProperty(Source, 'Items');
    if AObject <> nil then
      if AObject is TStrings then
        for I := 0 to TStrings(AObject).Count - 1 do
          Items.Add(TStrings(AObject)[I]);
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
    PopupAlignment := ConvertAlignment(GetEnumProperty(Source, 'PopupAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    Revertable := GetBooleanProperty(Source, 'Revertable', Revertable);
    if GetBooleanProperty(Source, 'DropDownListStyle', False) then
      DropDownListStyle := lsEditFixedList
    else
      DropDownListStyle := lsEditList;
  end;
  inherited DoImport;
end;

function TcxComboBoxPropertiesConverter.GetDestination: TcxComboBoxProperties;
begin
  Result := inherited Destination as TcxComboBoxProperties;
end;

{ TcxCurrencyEditPropertiesConverter }

procedure TcxCurrencyEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    DecimalPlaces := GetIntegerProperty(Source, 'DecimalPlaces', DecimalPlaces);
    DisplayFormat := GetStringProperty(Source, 'DisplayFormat', DisplayFormat);
    MaxValue := GetFloatProperty(Source, 'MaxValue', MaxValue);
    MinValue := GetFloatProperty(Source, 'MinValue', MinValue);
    Nullable := GetBooleanProperty(Source, 'Nullable', Nullable);
    Nullstring := GetStringProperty(Source, 'NullString', Nullstring);
    UseThousandSeparator := GetBooleanProperty(Source, 'UseThousandSeparator', UseThousandSeparator);
  end;
  inherited DoImport;
end;

function TcxCurrencyEditPropertiesConverter.GetDestination: TcxCurrencyEditProperties;
begin
  Result := inherited Destination as TcxCurrencyEditProperties;
end;

{ TcxDateEditPropertiesConverter }

procedure TcxDateEditPropertiesConverter.DoImport;
var
  ADateButtons: TStringList;
  AcxDateButtons: TDateButtons;
  I: Integer;
  ADateOnError: string;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ButtonGlyph.Assign(GetClassProperty(Source, 'ButtonGlyph') as TBitmap);
    ADateButtons := TStringList.Create;
    try
      GetSetProperty(Source, 'DateButtons', ADateButtons);
      ADateButtons.Sort;
      AcxDateButtons := [];
      if ADateButtons.Find('btnToday', I) then
        Include(AcxDateButtons, btnToday);
      if ADateButtons.Find('btnClear', I) then
        Include(AcxDateButtons, btnClear);
      DateButtons := AcxDateButtons;
    finally
      ADateButtons.Free;
    end;
    if GetBooleanProperty(Source, 'DateValidation') then
      DateOnError := deNoChange
    else
    begin
      ADateOnError := GetEnumProperty(Source, 'DateOnError');
      if ADateOnError = 'deToday'then
        DateOnError := deToday
      else if ADateOnError = 'deNull' then
        DateOnError := deNull;
    end;
    SaveTime := GetBooleanProperty(Source, 'SaveTime', SaveTime);
    if GetBooleanProperty(Source, 'UseEditMask', False) then
      InputKind := ikMask
    else
      InputKind := ikRegExpr;
  end;
  inherited DoImport;
end;

function TcxDateEditPropertiesConverter.GetDestination: TcxDateEditProperties;
begin
  Result := inherited Destination as TcxDateEditProperties;
end;

{ TcxHyperLinkEditPropertiesConverter }

procedure TcxHyperLinkEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    SingleClick := GetBooleanProperty(Source, 'SingleClick', SingleClick);
    StartKey := GetIntegerProperty(Source, 'StartKey', StartKey);
  end;
  inherited DoImport;
end;

function TcxHyperLinkEditPropertiesConverter.GetDestination: TcxHyperLinkEditProperties;
begin
  Result := inherited Destination as TcxHyperLinkEditProperties;
end;

{ TcxImagePropertiesConverter }

procedure TcxImagePropertiesConverter.DoImport;
var
  AObject: TObject;
  AGlyph: TObject;
  AButtons: TStringList;
  AMenuItems: TcxPopupMenuItems;
  I: Integer;
begin
  with Destination do
  begin
    Center := GetBooleanProperty(Source, 'Center', Center);
    CustomFilter := GetStringProperty(Source, 'CustomFilter', CustomFilter);
    GraphicTransparency := ConvertPictureTransparency(
      GetEnumProperty(Source, 'GraphicTransparency'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    Stretch := GetBooleanProperty(Source, 'Stretch', Stretch);
    AObject := GetClassProperty(Source, 'PopupToolBar');
    if AObject <> nil then
    begin
      PopupMenuLayout.CustomMenuItemCaption := GetStringProperty(AObject,
        'CustomButtonCaption', PopupMenuLayout.CustomMenuItemCaption);
      AGlyph := GetClassProperty(AObject, 'CustomButtonGlyph');
      PopupMenuLayout.CustomMenuItemGlyph.Assign(AGlyph as TBitmap);
      AButtons := TStringList.Create;
      try
        GetSetProperty(AObject, 'Buttons', AButtons);
        AButtons.Sort;
        AMenuItems := [];
        if AButtons.Find('ptbCut', I) then
          Include(AMenuItems, pmiCut);
        if AButtons.Find('ptbCopy', I) then
          Include(AMenuItems, pmiCopy);
        if AButtons.Find('ptbPaste', I) then
          Include(AMenuItems, pmiPaste);
        if AButtons.Find('ptbDelete', I) then
          Include(AMenuItems, pmiDelete);
        if AButtons.Find('ptbLoad', I) then
          Include(AMenuItems, pmiLoad);
        if AButtons.Find('ptbSave', I) then
          Include(AMenuItems, pmiSave);
        if AButtons.Find('ptbCustom', I) then
          Include(AMenuItems, pmiCustom);
        PopupMenuLayout.MenuItems := AMenuItems;
      finally
        AButtons.Free;
      end;
    end;
  end;
  inherited DoImport;
end;

function TcxImagePropertiesConverter.GetDestination: TcxImageProperties;
begin
  Result := inherited Destination as TcxImageProperties;
end;

{ TcxImageComboBoxPropertiesConverter }

procedure TcxImageComboBoxPropertiesConverter.DoImport;
var
  AObject: TObject;
  AItem: TcxImageComboBoxItem;
  AdxImageIndexes: TObject;
  AdxValues: TObject;
  AdxDescriptions: TObject;
  I: Integer;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ButtonGlyph.Assign(GetClassProperty(Source, 'ButtonGlyph') as TBitmap);
    DropDownRows := GetIntegerProperty(Source, 'DropDownRows', DropDownRows);
    AObject := GetClassProperty(Source, 'Images');
    if AObject <> nil then
      Images := AObject as TCustomImageList
    else
      Images := nil;
    AObject := GetClassProperty(Source, 'LargeImages');
    if AObject <> nil then
      LargeImages := AObject as TCustomImageList
    else
      LargeImages := nil;
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    MultiLineText := GetBooleanProperty(Source,'MultiLineText', MultiLineText);
    ShowDescriptions := GetBooleanProperty(Source, 'ShowDescription', ShowDescriptions);
    AdxImageIndexes := GetClassProperty(Source, 'ImageIndexes');
    AdxValues := GetClassProperty(Source, 'Values');
    AdxDescriptions := GetClassProperty(Source, 'Descriptions');
    if (AdxImageIndexes <> nil) and (AdxValues <> nil) and
      (AdxDescriptions <> nil) then
    begin
      if (AdxImageIndexes is TStrings) and (AdxValues is TStrings) and
        (AdxDescriptions is TStrings) then
      begin
        for I := 0 to TStrings(AdxImageIndexes).Count - 1 do
        begin
          AItem := Items.Add as TcxImageComboBoxItem;
          AItem.ImageIndex := StrToInt(TStrings(AdxImageIndexes)[I]);
          if I < TStrings(AdxDescriptions).Count then
            AItem.Description := TStrings(AdxDescriptions)[I];
          if I < TStrings(AdxValues).Count then
            AItem.Value := TStrings(AdxValues)[I];
        end;
      end;
    end;
  end;
  inherited DoImport;
end;

function TcxImageComboBoxPropertiesConverter.GetDestination: TcxImageComboBoxProperties;
begin
  Result := inherited Destination as TcxImageComboBoxProperties;
end;

{ TcxLookupComboBoxPropertiesConverter }

procedure TcxLookupComboBoxPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    ClearKey := GetIntegerProperty(Source, 'ClearKey', ClearKey);
    ImmediateDropDown := GetBooleanProperty(Source, 'ImmediateDropDown', ImmediateDropDown);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    ButtonGlyph.Assign(GetClassProperty(Source, 'ButtonGlyph') as TBitmap);
    DropDownRows := GetIntegerProperty(Source, 'DropDownRows', DropDownRows);
    DropDownWidth := GetIntegerProperty(Source, 'DropDownWidth', DropDownWidth);
    ListFieldNames := GetStringProperty(Source, 'ListFieldName', ListFieldNames);
    PopupAlignment := ConvertAlignment(GetEnumProperty(Source, 'PopupAlignment'));
    ListOptions.ShowHeader := False;
  end;
  inherited DoImport;
end;

function TcxLookupComboBoxPropertiesConverter.GetDestination: TcxLookupComboBoxProperties;
begin
  Result := inherited Destination as TcxLookupComboBoxProperties;
end;

{ TcxMaskEditPropertiesConverter }

procedure TcxMaskEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    EditMask := GetStringProperty(Source, 'EditMask', EditMask);
    IgnoreMaskBlank := GetBooleanProperty(Source, 'IgnoreMaskBlank', IgnoreMaskBlank);
    MaskKind := emkStandard;
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
  end;
  inherited DoImport;
end;

function TcxMaskEditPropertiesConverter.GetDestination: TcxMaskEditProperties;
begin
  Result := inherited Destination as TcxMaskEditProperties;
end;

{ TcxMemoPropertiesConverter }

procedure TcxMemoPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
    ScrollBars := ConvertScrollStyle(GetEnumProperty(Source, 'ScrollBars'));
    WantReturns := GetBooleanProperty(Source, 'WantReturns', WantReturns);
    WantTabs := GetBooleanProperty(Source, 'WantTabs', WantTabs);
    WordWrap := GetBooleanProperty(Source, 'WordWrap', WordWrap);
  end;
  inherited DoImport;
end;

function TcxMemoPropertiesConverter.GetDestination: TcxMemoProperties;
begin
  Result := inherited Destination as TcxMemoProperties;
end;

{ TcxMRUEditPropertiesConverter }

procedure TcxMRUEditPropertiesConverter.DoImport;
var
  AObject: TObject;
  AItems: TStringList;
  I: Integer;
begin
  with Destination do
  begin
    AItems := GetClassProperty(Source, 'Items') as TStringList;
    if AItems <> nil then
      for I := 0 to AItems.Count - 1 do
        LookupItems.Add(AItems[I]);
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    AObject := GetClassProperty(Source, 'ButtonGlyph');
    ButtonGlyph.Assign(AObject as TBitmap);
    DropDownRows := GetIntegerProperty(Source, 'DropDownRows', DropDownRows);
    ImmediateDropDown := GetBooleanProperty(Source, 'ImmediateDropDown', ImmediateDropDown);
    MaxItemCount := GetIntegerProperty(Source, 'MaxItemCount', MaxItemCount);
    ShowEllipsis := GetBooleanProperty(Source, 'ShowEllipsis', ShowEllipsis);
  end;
  inherited DoImport;
end;

function TcxMRUEditPropertiesConverter.GetDestination: TcxMRUEditProperties;
begin
  Result := inherited Destination as TcxMRUEditProperties;
end;

{ TcxPopupEditPropertiesConverter }

procedure TcxPopupEditPropertiesConverter.DoImport;
var
  AObject: TObject;
  APanelStyle: string;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
    PopupAutoSize := GetBooleanProperty(Source, 'PopupAutoSize', PopupAutoSize);
    PopupClientEdge := GetBooleanProperty(Source, 'PopupFormClientEdge', PopupClientEdge);
    AObject := GetClassProperty(Source, 'PopupControl');
    if AObject <> nil then
      PopupControl := AObject as TControl
    else
      PopupControl := nil;
    PopupHeight := GetIntegerProperty(Source, 'PopupHeight', PopupHeight);
    PopupMinHeight := GetIntegerProperty(Source, 'PopupMinHeight', PopupMinHeight);
    PopupMinWidth := GetIntegerProperty(Source, 'PopupMinWidth', PopupMinWidth);
    PopupWidth := GetIntegerProperty(Source, 'PopupWidth', PopupWidth);
    PopupSizeable := GetBooleanProperty(Source, 'PopupFormSizeable', PopupSizeable);
    APanelStyle := GetEnumProperty(Source, 'PopupFormBorderStyle');
    PopupSysPanelStyle := APanelStyle = 'pbsSysPanel';
  end;
  inherited DoImport;
end;

function TcxPopupEditPropertiesConverter.GetDestination: TcxPopupEditProperties;
begin
  Result := inherited Destination as TcxPopupEditProperties;
end;

{ TcxSpinEditPropertiesConverter }

procedure TcxSpinEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    Increment := GetFloatProperty(Source, 'Increment', Increment);
    MaxValue := GetFloatProperty(Source, 'MaxValue', MaxValue);
    MinValue := GetFloatProperty(Source, 'MinValue', MinValue);
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
  end;
  inherited DoImport;
end;

function TcxSpinEditPropertiesConverter.GetDestination: TcxSpinEditProperties;
begin
  Result := inherited Destination as TcxSpinEditProperties;
end;

{ TcxTextEditPropertiesConverter }

procedure TcxTextEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    CharCase := ConvertCharCase(GetEnumProperty(Source, 'CharCase'));
    MaxLength := GetIntegerProperty(Source, 'MaxLength', MaxLength);
    OEMConvert := GetBooleanProperty(Source, 'OEMConvert', OEMConvert);
  end;
  inherited DoImport;
end;

function TcxTextEditPropertiesConverter.GetDestination: TcxTextEditProperties;
begin
  Result := inherited Destination as TcxTextEditProperties;
end;

{ TcxTimeEditPropertiesConverter }

procedure TcxTimeEditPropertiesConverter.DoImport;
begin
  with Destination do
  begin
    Alignment.Horz := ConvertAlignment(GetEnumProperty(Source, 'Alignment'));
    Alignment.Vert := ConvertVertAlignment(GetEnumProperty(Source, 'VertAlignment'));
    ReadOnly := GetBooleanProperty(Source, 'ReadOnly', ReadOnly);
    TimeFormat := ConvertTimeFormat(GetEnumProperty(Source, 'TimeEditFormat'));
  end;
  inherited DoImport;
end;

function TcxTimeEditPropertiesConverter.ConvertTimeFormat(const AFormat: string): TcxTimeEditTimeFormat;
begin
  if AFormat = 'tfHour' then
    Result := tfHour
  else if AFormat = 'tfHourMin' then
    Result := tfHourMin
  else if AFormat = 'tfHourMinSec' then
    Result := tfHourMinSec
  else
    Result := tfHour;
end;

function TcxTimeEditPropertiesConverter.GetDestination: TcxTimeEditProperties;
begin
  Result := inherited Destination as TcxTimeEditProperties;
end;

end.
