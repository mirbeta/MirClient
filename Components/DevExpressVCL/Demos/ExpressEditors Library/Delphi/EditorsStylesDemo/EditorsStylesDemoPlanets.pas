unit EditorsStylesDemoPlanets;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, cxPropertiesStore, cxContainer, cxEdit, cxHint, cxLabel,
  cxControls, cxSplitter, ExtCtrls, ImgList, cxListView, cxMCListBox,
  ComCtrls, StdCtrls, EditorsStylesDemoBase, cxTextEdit, cxMemo, cxClasses;

type
  TEditorsStylesDemoPlanetsFrame = class(TEditorsStylesDemoBaseFrame)
    pnlPlanets: TPanel;
    pnlSatellites: TPanel;
    cxSplitter: TcxSplitter;
    pnlPlanetsLbl: TPanel;
    pnlSatellitesLbl: TPanel;
    lblPlanets: TcxLabel;
    lblSatellites: TcxLabel;
    cxMCListBox: TcxMCListBox;
    cxListView: TcxListView;
    ImageList: TImageList;
    procedure cxMCListBoxClick(Sender: TObject);
    procedure cxListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
  private
    FRecordValues: TStringList;
  protected
    function GetValue(ARecordIndex, AColIndex: Integer): string;
    function GetIndexByName(AName: string): Integer; virtual;
    procedure InitCurrentRecordValues(ARecord: string);
    procedure UpdateSatellites;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Name: string; override;
    function BriefName: string; override;
    function StylesIniPath: string; override;
    function GetStyleBackgroundColor: TColor; override;
    function Description: String; override;
  end;

var
  EditorsStylesDemoPlanetsFrame: TEditorsStylesDemoPlanetsFrame;

implementation

{$R *.dfm}

const
  cxPlanetsFileName = 'nineplanets.txt';

{ TEditorsStylesDemoDemoPlanetsFrame }

constructor TEditorsStylesDemoPlanetsFrame.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FRecordValues := TStringList.Create;
  cxMCListBox.Items.LoadFromFile(cxPlanetsFileName);
  InitCurrentRecordValues(cxMCListBox.Items[0]);
  cxMCListBox.Items.Delete(0);
  for I := 0 to cxMCListBox.HeaderSections.Count - 1 do
    cxMCListBox.HeaderSections[I].Text := FRecordValues.Strings[I];
  HintStyle := hcstBlueSlideUp;
  if cxMCListBox.Count > 0 then
  begin
    cxMCListBox.ItemIndex := 0;
    cxMCListBoxClick(nil);
  end;
  FDisplayStyle := shtLightGray;
  FTempDisplayStyle := shtLightGray;
end;

destructor TEditorsStylesDemoPlanetsFrame.Destroy;
begin
  FreeAndNil(FRecordValues);
  inherited Destroy;
end;

procedure TEditorsStylesDemoPlanetsFrame.cxMCListBoxClick(Sender: TObject);
begin
  UpdateSatellites;
end;

procedure TEditorsStylesDemoPlanetsFrame.cxListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: String);
var
  I, ARecordIndex: Integer;
begin
  InfoTip := '';
  ARecordIndex := GetIndexByName(Item.Caption);
  for I := 1 to cxMCListBox.HeaderSections.Count - 1 do
  begin
    InfoTip := InfoTip + cxMCListBox.HeaderSections[I].Text + ': ' +
      GetValue(ARecordIndex, I) + #10#13;
  end;
end;

function TEditorsStylesDemoPlanetsFrame.Name: string;
begin
  Result := 'Solar System';
end;

function TEditorsStylesDemoPlanetsFrame.BriefName: string;
begin
  Result := 'Solar';
end;

function TEditorsStylesDemoPlanetsFrame.StylesIniPath: string;
begin
  Result := 'StylesFrmSolarSystem\';
end;

function TEditorsStylesDemoPlanetsFrame.GetStyleBackgroundColor: TColor;
begin
  Result := cxListView.Style.Color;
end;

function TEditorsStylesDemoPlanetsFrame.Description: String;
begin
  Result := 'Solar System Notes';
end;

function TEditorsStylesDemoPlanetsFrame.GetValue(ARecordIndex,
  AColIndex: Integer): string;
begin
  InitCurrentRecordValues(cxMCListBox.Items[ARecordIndex]);
  Result := '-1';
  if FRecordValues.Count > AColIndex then
    Result := FRecordValues.Strings[AColIndex];
end;

function TEditorsStylesDemoPlanetsFrame.GetIndexByName(
  AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to cxMCListBox.Count - 1 do
   if GetValue(I, 0) = AName then
   begin
     Result := I;
     Break;
   end;
end;

procedure TEditorsStylesDemoPlanetsFrame.InitCurrentRecordValues(
  ARecord: string);
var
 I: Integer;
 AString: string;
 ADelimiter: Char;
begin
 ADelimiter := cxMCListBox.Delimiter;
 AString := '';
 for I := 1 to Length(ARecord) do
   if ARecord[I] = ADelimiter then
     AString := AString + #13#10
   else
     AString := AString + ARecord[I];
 FRecordValues.Clear;
 FRecordValues.Text := AString;
end;

procedure TEditorsStylesDemoPlanetsFrame.UpdateSatellites;
var
  APlanetName: string;
  I: Integer;
begin
  APlanetName := GetValue(cxMCListBox.ItemIndex, 0);
  cxListView.Items.Clear;
  cxListView.ViewStyle := vsIcon;
  for I := 0 to cxMCListBox.Count-1 do
    if GetValue(I, 2) = APlanetName then
      with cxListView.Items.Add do
      begin
        Caption := GetValue(I, 0);
        ImageIndex := StrToInt(GetValue(I, 6));
      end;
  if cxListView.Items.Count = 0 then
  begin
    with cxListView.Items.Add do
    begin
      Caption := 'There are no satellites';
    	ImageIndex := -1;
    end;
    cxListView.ViewStyle := vsList;
  end;
end;

initialization
  EditorsStylesDemoFrameManager.RegisterFrameClass(TEditorsStylesDemoPlanetsFrame);

end.
