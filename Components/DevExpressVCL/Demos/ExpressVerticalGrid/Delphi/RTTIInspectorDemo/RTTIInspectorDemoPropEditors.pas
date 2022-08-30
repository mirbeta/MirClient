unit RTTIInspectorDemoPropEditors;

interface

uses cxOI, cxEdit, ImgList, cxImageComboBox, Controls;

type
  TcxImageIndexProperty = class(TcxIntegerProperty)
  public
    procedure AdjustInnerEditProperties(AProperties: TcxCustomEditProperties); override;
    function GetImages: TCustomImageList; virtual;
    function GetAttributes: TcxPropertyAttributes; override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses SysUtils, cxVGrid;

resourcestring
  sNoImage = 'no image';

{ TcxImageIndexProperty }

function TcxImageIndexProperty.GetAttributes: TcxPropertyAttributes;
begin
  Result := [ipaMultiSelect, ipaAutoUpdate];
end;

procedure TcxImageIndexProperty.AdjustInnerEditProperties(
  AProperties: TcxCustomEditProperties);
var
  i: Integer;
  AComboBoxItem: TcxImageComboBoxItem;
  AImageComboBoxProperties: TcxImageComboBoxProperties;
  AImages: TCustomImageList;
  procedure AssignImages;
  begin
    if AImages.Height = 16 then
      AImageComboBoxProperties.Images := AImages
    else
      AImageComboBoxProperties.LargeImages := AImages
  end;

  function GetImageList: TCustomImageList;
  begin
    if AImageComboBoxProperties.Images <> nil then
      Result := AImageComboBoxProperties.Images
    else
      Result := AImageComboBoxProperties.LargeImages;
  end;

begin
  AImageComboBoxProperties := AProperties as TcxImageComboBoxProperties;
  AImages := GetImages;
  if AImages <> nil then
  begin
    AssignImages;
    AImageComboBoxProperties.Items.Clear;
    for i:=0 to GetImageList.Count - 1 do
    begin
      AComboBoxItem := AImageComboBoxProperties.Items.Add as TcxImageComboBoxItem;
      AComboBoxItem.ImageIndex := i;
      AComboBoxItem.Value := i;
      AComboBoxItem.Description := IntToStr(i);
    end;
  end;
  AComboBoxItem := AImageComboBoxProperties.Items.Add as TcxImageComboBoxItem;
  AComboBoxItem.ImageIndex := -1;
  AComboBoxItem.Value := Integer(-1);
  AComboBoxItem.Description := sNoImage;
  AImageComboBoxProperties.OnEditValueChanged := nil;
end;

procedure TcxImageIndexProperty.SetValue(const Value: string);
begin
  if Value = sNoImage then
    SetOrdValue(-1)
  else inherited SetValue(Value);
end;

function TcxImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := nil;
  if GetComponent(0) is TcxCustomEditorRowProperties then
     Result := TcxEditorRowProperties(GetComponent(0)).Row.VerticalGrid.Images;
end;

initialization
  cxRegisterPropertyEditor(TypeInfo(TImageIndex), TcxCustomEditorRowProperties, 'ImageIndex', TcxImageIndexProperty);
  cxRegisterEditPropertiesClass(TcxImageIndexProperty, TcxImageComboBoxProperties);

end.
