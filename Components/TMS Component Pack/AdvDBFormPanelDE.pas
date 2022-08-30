{***************************************************************************}
{ TAdvDBFormPanel designer                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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

unit AdvDBFormPanelDE;

interface

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, AdvDBFormPanel, DesignIntf, DesignEditors, ContNrs;

type

  TAdvDBFormPanelEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


  TAdvDBFormBoxEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TLayoutItemFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


implementation

uses
  AdvDBFormLayouter, SysUtils, DB, DBCtrls;

type
  TVCLDesignCreator = class(TInterfacedObject, IComponentCreator)
  private
    FDesigner: IDesigner;
  public
    constructor Create(Designer: IDesigner);
    function CreateComponent(AClass: TControlClass; AOwner: TComponent): TControl;
    procedure DeleteComponent(AComponent: TComponent);
  end;

  TAdvDBFormPanelX = class(TAdvDBFormPanel);

  TAdvDBFormBoxX = class(TAdvDBFormBox);

{ TAdvDBFormPanelEditor }

procedure TAdvDBFormPanelEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'LAYOUT') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvDBFormPanelEditor.ExecuteVerb(Index: Integer);
var
  VCLDesignCreator: TVCLDesignCreator;
  Panel: TAdvDBFormPanelX;
  Bounds: TRect;
  i: integer;
begin
  inherited;

  case Index of
  0:
    begin
      Panel := TAdvDBFormPanelX(Component);

      if Assigned(Panel) then
      begin
        if not Assigned(Panel.DataSource) then
        begin
          ShowMessage('Datasource not assigned');
          Exit;
        end;

        if not Assigned(Panel.DataSource.DataSet) then
        begin
          ShowMessage('Dataset not assigned to datasource');
          Exit;
        end;

        if not Panel.DataSource.DataSet.Active then
        begin
          ShowMessage('Dataset not active');
          Exit;
        end;

        if Panel.ControlCount > 0 then
        begin
          ShowMessage('Panel already has controls');
          Exit;
        end;

        Panel.Layout.BeginUpdate;

        Panel.RemoveControls;

        if Panel.CustomItems then
        begin
          Panel.InitFields;
        end
        else
        begin
          Panel.Layout.Items.Clear;
          Panel.InitLayout;
        end;

        Panel.Layout.EndUpdate;

        Bounds := Panel.ClientRect;

        VCLDesignCreator := TVCLDesignCreator.Create(Designer);

        Panel.Layouter.DoLayout(Component as TWinControl, Panel.DataSource, Panel.Layout, Bounds, VCLDesignCreator);

        Designer.Modified;
      end;
    end;

  1:
    begin
      Panel := TAdvDBFormPanelX(Component);

      Panel.Layout.BeginUpdate;
      Panel.RemoveLayout;
      Panel.AutoLayout := false;
      Panel.Layout.EndUpdate;

      for i := Panel.ControlCount - 1 downto 0 do
      begin
        Panel.Controls[i].Free;
      end;

      Designer.Modified;
    end;
  2:
    begin
      Panel := TAdvDBFormPanelX(Component);

      if Assigned(Panel) then
      begin
        if not Assigned(Panel.DataSource) then
        begin
          ShowMessage('Datasource not assigned');
          Exit;
        end;

        if not Assigned(Panel.DataSource.DataSet) then
        begin
          ShowMessage('Dataset not assigned to datasource');
          Exit;
        end;

        if not Panel.DataSource.DataSet.Active then
        begin
          ShowMessage('Dataset not active');
          Exit;
        end;

        if Panel.AutoLayout then
        begin
          Panel.Layout.BeginUpdate;
          Panel.RemoveLayout;
          Panel.AutoLayout := false;
          Panel.Layout.EndUpdate;
        end;

        Panel.Layout.BeginUpdate;

        Panel.RemoveControls;
        for i := Panel.ControlCount - 1 downto 0 do
        begin
          Panel.Controls[i].Free;
        end;

        if Panel.CustomItems then
        begin
          Panel.InitFields;
        end
        else
        begin
          Panel.Layout.Items.Clear;
          Panel.InitLayout;
        end;

        Panel.Layout.EndUpdate;

        Bounds := Panel.ClientRect;

        VCLDesignCreator := TVCLDesignCreator.Create(Designer);

        Panel.Layouter.DoLayout(Component as TWinControl, Panel.DataSource, Panel.Layout, Bounds, VCLDesignCreator);

        Designer.Modified;
      end;
    end;
  end;
end;

function TAdvDBFormPanelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add DB controls';
  1: Result := 'Remove DB controls';
  2: Result := 'Relayout DB controls';
  end;
end;

function TAdvDBFormPanelEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TVCLDesignCreator }

constructor TVCLDesignCreator.Create(Designer: IDesigner);
begin
  FDesigner := Designer;
end;

function TVCLDesignCreator.CreateComponent(AClass: TControlClass;
  AOwner: TComponent): TControl;
begin
  Result := TControl(FDesigner.CreateComponent(AClass, AOwner, 0,0, 0,0));
end;

procedure TVCLDesignCreator.DeleteComponent(AComponent: TComponent);
begin
  AComponent.Free;
  FDesigner.Modified;
end;

{ TAdvDBFormBoxEditor }

procedure TAdvDBFormBoxEditor.ExecuteVerb(Index: Integer);
var
  VCLDesignCreator: TVCLDesignCreator;
  Panel: TAdvDBFormPanelX;
  Box: TAdvDBFormBoxX;
  Bounds: TRect;
  i: integer;
begin
  inherited;

  case Index of
  0:
    begin
      Box := TAdvDBFormBoxX(Component);

      if Assigned(Box) then
      begin
        if not Assigned(Box.DataSource) then
        begin
          ShowMessage('Datasource not assigned');
          Exit;
        end;

        if not Assigned(Box.DataSource.DataSet) then
        begin
          ShowMessage('Dataset not assigned to datasource');
          Exit;
        end;

        if not Box.DataSource.DataSet.Active then
        begin
          ShowMessage('Dataset not active');
          Exit;
        end;

        Panel := TAdvDBFormPanelX(Box.DBForm);

        if Panel.ControlCount > 0 then
        begin
          ShowMessage('Panel already has controls');
          Exit;
        end;

        Panel.Layout.BeginUpdate;

        Panel.RemoveControls;

        if Panel.CustomItems then
        begin
          Panel.InitFields;
        end
        else
        begin
          Panel.Layout.Items.Clear;
          Panel.InitLayout;
        end;

        Panel.Layout.EndUpdate;

        Bounds := Panel.ClientRect;

        VCLDesignCreator := TVCLDesignCreator.Create(Designer);

        Panel.Layouter.DoLayout(Panel, Box.DataSource, Box.Layout, Bounds, VCLDesignCreator);

        Designer.Modified;
      end;
    end;
  1:
    begin

      Box := TAdvDBFormBoxX(Component);

      if Assigned(Box) then
      begin
        Panel := TAdvDBFormPanelX(Box.DBForm);

        Panel.Layout.BeginUpdate;
        Panel.RemoveLayout;
        Panel.AutoLayout := false;
        Panel.Layout.EndUpdate;

        for i := Panel.ControlCount - 1 downto 0 do
        begin
          Panel.Controls[i].Free;
        end;

        Designer.Modified;
      end;
    end;
  2:
    begin
      Box := TAdvDBFormBoxX(Component);

      if Assigned(Box) then
      begin
        if not Assigned(Box.DataSource) then
        begin
          ShowMessage('Datasource not assigned');
          Exit;
        end;

        if not Assigned(Box.DataSource.DataSet) then
        begin
          ShowMessage('Dataset not assigned to datasource');
          Exit;
        end;

        if not Box.DataSource.DataSet.Active then
        begin
          ShowMessage('Dataset not active');
          Exit;
        end;

        Panel := TAdvDBFormPanelX(Box.DBForm);

        Panel.Layout.BeginUpdate;

        Panel.RemoveControls;
        for i := Panel.ControlCount - 1 downto 0 do
        begin
          Panel.Controls[i].Free;
        end;

        if Panel.CustomItems then
        begin
          Panel.InitFields;
        end
        else
        begin
          Panel.Layout.Items.Clear;
          Panel.InitLayout;
        end;

        Panel.Layout.EndUpdate;

        Bounds := Panel.ClientRect;

        VCLDesignCreator := TVCLDesignCreator.Create(Designer);

        Panel.Layouter.DoLayout(Panel, Box.DataSource, Box.Layout, Bounds, VCLDesignCreator);

        Designer.Modified;
      end;
    end;


  end;
end;

function TAdvDBFormBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Add DB controls';
  1: Result := 'Remove DB controls';
  2: Result := 'Relayout DB controls';
  end;
end;

function TAdvDBFormBoxEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TLayoutItemFieldNameProperty }

function TLayoutItemFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TLayoutItemFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  LayoutItem: TLayoutItem;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
  li: TLayout;
  lp: TAdvDBFormPanel;
begin
  LayoutItem := (GetComponent(0) as TLayoutItem);

  li := TLayout((LayoutItem.Collection as TLayoutItems).Owner);
  lp := TAdvDBFormPanel(li.Owner);

  FDataSource := lp.DataSource;

  if not Assigned(fDataSource) then
    Exit;

  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then
    Exit;

  st := TStringList.Create;
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do
    proc(st.Strings[i-1]);
  st.Free;
end;

end.
