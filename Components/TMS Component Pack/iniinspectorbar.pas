{***************************************************************************}
{ TINIInspectorBar component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

{$I TMSDEFS.INC}

unit INIInspectorBar;

interface

uses
  InspectorBar, Classes, Messages, Windows, Controls, SysUtils, Graphics, INIFiles, Dialogs;

type
  TINIInspectorItem = class(TInspectorItem)
  private
    function GetCaptionEx: string;
    procedure SetCaptionEx(const Value: string);
  protected
    procedure EditStart; override;
    procedure EditStop; override;
    procedure EditChange; override;
    procedure Refresh;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Caption: string read GetCaptionEx write SetCaptionEx;
  end;

  TINIInspectorItems = class(TInspectorItems)
  private
    function GetItem(Index: Integer): TINIInspectorItem;
    procedure SetItem(Index: Integer; const Value: TINIInspectorItem);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TINIInspectorItem;
    function Insert(index: Integer): TINIInspectorItem;
    property Items[Index: Integer]: TINIInspectorItem read GetItem write SetItem; default;
  published
  end;

  TINIInspectorPanel = class(TInspectorPanel)
  private
    FINISection: string;
    procedure SetINISection(const Value: string);
  public
    function CreateItems: TInspectorItems; override;
    constructor Create(Collection: TCollection); override;
  published
    property INISection: string read FINISection write SetINISection;
  end;

  TINIInspectorPanels = class(TInspectorPanels)
  private
    function GetItem(Index: Integer): TINIInspectorPanel;
    procedure SetItem(Index: Integer; const Value: TINIInspectorPanel);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TINIInspectorPanel;
    function Insert(index: Integer): TINIInspectorPanel;
    property Items[Index: Integer]: TINIInspectorPanel read GetItem write SetItem; default;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TINIInspectorBar = class(TInspectorBar)
  private
    FAllSections: Boolean;
    FINIFile: string;
    procedure SetAllSections(const Value: Boolean);
    procedure SetINIFile(const Value: string);
    function GetPanels: TINIInspectorPanels;
    procedure SetPanels(const Value: TINIInspectorPanels);
  public
    procedure Loaded; override;
    function CreatePanels: TInspectorPanels; override;
    procedure StartEdit(InspectorItem: TInspectorItem); override;
    procedure StopEdit(InspectorItem: TInspectorItem); override;
  published
    property AllSections: Boolean read FAllSections write SetAllSections;
    property INIFile: string read FINIFile write SetINIFile;
    property Panels: TINIInspectorPanels read GetPanels write SetPanels;
  end;

implementation

{ TINIInspectorBar }

function TINIInspectorBar.CreatePanels: TInspectorPanels;
begin
  Result := TINIInspectorPanels.Create(Self);
end;

function TINIInspectorBar.GetPanels: TINIInspectorPanels;
begin
  Result := TINIInspectorPanels(inherited Panels);
end;

procedure TINIInspectorBar.Loaded;
var
  i,j: Integer;
begin
  inherited;
  for i := 1 to Panels.Count do
  begin
    for j := 1 to Panels[i - 1].Items.Count do
      TINIInspectorItem(Panels[i - 1].Items[j - 1]).Refresh;
  end;

end;

procedure TINIInspectorBar.SetAllSections(const Value: Boolean);
var
  i,j,k: Integer;
  IniFile: TIniFile;
  sls,slk: TStringList;
  INIPanel: TINIInspectorPanel;
begin

  if (Value <> FAllSections) and Value and (FINIFile <> '') then
  begin
    Panels.Clear;

    sls := TStringList.Create;

    IniFile := TIniFile.Create(FINIFile);

    IniFile.ReadSections(sls);

    for i := 1 to sls.Count do
    begin
      INIPanel := Panels.Add;
      INIPanel.Caption := sls.Strings[i - 1];
      INIPanel.INISection := sls.Strings[i - 1];
      INIPanel.Style := psProperties;
      INIPanel.Color := clWindow;
      INIPanel.ItemHeight := 22;
      INIPanel.GridLines := True;

      slk := TStringList.Create;
      Inifile.ReadSectionValues(sls.Strings[i - 1],slk);

      for j := 1 to slk.Count do
        with INIPanel.Items.Add do
        begin
          PropertyType := ptText;
          k := Pos('=',slk.Strings[j - 1]);
          if k > 0 then
          begin
            Caption := Copy(slk.Strings[j - 1],1,k - 1);
            TextValue := Copy(slk.Strings[j - 1], k + 1, Length(slk.Strings[j - 1]));
          end;
        end;
      slk.Free;
    end;

    sls.Free;
    IniFile.Free;
  end
  else
    if (not Value) and (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      Panels.Clear;

  FAllSections := Value;
end;

procedure TINIInspectorBar.SetINIFile(const Value: string);
begin
  FINIFile := Value;
end;

procedure TINIInspectorBar.SetPanels(const Value: TINIInspectorPanels);
begin
  inherited Panels := Value;
end;

procedure TINIInspectorBar.StartEdit(InspectorItem: TInspectorItem);
begin
  inherited;
end;

procedure TINIInspectorBar.StopEdit(InspectorItem: TInspectorItem);
var
  IniFile: TIniFile;
  EditState: boolean;
begin
  EditState := Editing;

  inherited;

  if not Assigned(InspectorItem) then
    Exit;

  if not EditState then
    Exit;

  if FIniFile = '' then
    Exit;

  IniFile := TIniFile.Create(FIniFile);

  if InspectorItem.PropertyType in [ptText,ptValues] then
  begin
    IniFile.WriteString(TINIInspectorPanel(InspectorItem.InspectorPanel).INISection,
      TINIInspectorItem(InspectorItem).Caption,TINIInspectorItem(InspectorItem).TextValue);
  end;

  if InspectorItem.PropertyType in [ptInteger,ptIntSpin] then
  begin
    IniFile.WriteInteger(TINIInspectorPanel(InspectorItem.InspectorPanel).INISection,
      TINIInspectorItem(InspectorItem).Caption,TINIInspectorItem(InspectorItem).IntValue);
  end;

  if InspectorItem.PropertyType in [ptBoolean] then
  begin
    if TINIInspectorItem(InspectorItem).BoolValue then
      IniFile.WriteString(TINIInspectorPanel(InspectorItem.InspectorPanel).INISection,
        TINIInspectorItem(InspectorItem).Caption,CheckTrue)
    else
      IniFile.WriteString(TINIInspectorPanel(InspectorItem.InspectorPanel).INISection,
        TINIInspectorItem(InspectorItem).Caption,CheckFalse);
  end;

  IniFile.Free;
end;

{ TINIInspectorPanels }

function TINIInspectorPanels.Add: TINIInspectorPanel;
begin
  Result := TINIInspectorPanel(inherited Add);
end;

function TINIInspectorPanels.CreateItemClass: TCollectionItemClass;
begin
  Result := TINIInspectorPanel;
end;


function TINIInspectorPanels.GetItem(Index: Integer): TINIInspectorPanel;
begin
  Result := TINIInspectorPanel(inherited Items[Index]);
end;

function TINIInspectorPanels.Insert(index: Integer): TINIInspectorPanel;
begin
  Result := TINIInspectorPanel(inherited Insert(Index));
end;

procedure TINIInspectorPanels.SetItem(Index: Integer;
  const Value: TINIInspectorPanel);
begin
  inherited Items[Index] := Value;
end;

{ TINIInspectorPanel }

constructor TINIInspectorPanel.Create(Collection: TCollection);
begin
  inherited;
  Style := psProperties;
  ItemHeight := 28;
end;

function TINIInspectorPanel.CreateItems: TInspectorItems;
begin
  Result := TINIInspectorItems.Create(Self);
end;

procedure TINIInspectorPanel.SetINISection(const Value: string);
begin
  FINISection := Value;
end;

{ TINIInspectorItems }

function TINIInspectorItems.Add: TINIInspectorItem;
begin
  Result := TINIInspectorItem(inherited Add);
end;

function TINIInspectorItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TINIInspectorItem;
end;

function TINIInspectorItems.GetItem(Index: Integer): TINIInspectorItem;
begin
  Result := TINIInspectorItem(inherited Items[Index]);
end;

function TINIInspectorItems.Insert(index: Integer): TINIInspectorItem;
begin
  Result := TINIInspectorItem(inherited Insert(Index));
end;

procedure TINIInspectorItems.SetItem(Index: Integer;
  const Value: TINIInspectorItem);
begin
  inherited Items[Index] := Value;
end;

{ TINIInspectorItem }

constructor TINIInspectorItem.Create(Collection: TCollection);
begin
  inherited;
  PropertyType := ptText;
end;

destructor TINIInspectorItem.Destroy;
begin
  inherited;
end;

procedure TINIInspectorItem.EditChange;
begin
  inherited;
end;

procedure TINIInspectorItem.EditStart;
begin
  inherited;
end;

procedure TINIInspectorItem.EditStop;
begin
  inherited;
end;

function TINIInspectorItem.GetCaptionEx: string;
begin
  Result := inherited Caption;
end;

procedure TINIInspectorItem.Refresh;
var
  IniFile: TIniFile;
  inifn: string;
begin
  inifn := TINIInspectorBar(InspectorBar).IniFile;
  if (PropertyType = ptText) and (inifn <> '') then
  begin
    IniFile := TIniFile.Create(inifn);
    TextValue := IniFile.ReadString(TINIInspectorPanel(InspectorPanel).INISection,Caption,'');
    IniFile.Free;
  end;
  if (PropertyType = ptBoolean) and (inifn <> '') then
  begin
    IniFile := TIniFile.Create(inifn);
    TextValue := IniFile.ReadString(TINIInspectorPanel(InspectorPanel).INISection,Caption,'');
    BoolValue := TextValue = InspectorBar.CheckTrue;
    IniFile.Free;
  end;

  if (PropertyType in [ptInteger, ptIntSpin]) and (inifn <> '') then
  begin
    IniFile := TIniFile.Create(inifn);
    IntValue := IniFile.ReadInteger(TINIInspectorPanel(InspectorPanel).INISection,Caption,0);
    IniFile.Free;

  end;
end;

procedure TINIInspectorItem.SetCaptionEx(const Value: string);
begin
  inherited Caption := Value;
  Refresh;
end;

end.
