{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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
unit cxImportDialog;

{$I cxVer.inc}

interface

uses
  DesignIntf, Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, cxConverterFactory, TypInfo,
  cxStyles, cxDesignWindows;

type
  TcxImportDialogForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    pcStylePane: TPageControl;
    TabSheet2: TTabSheet;
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    cbImportStyles: TCheckBox;
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure cbImportStylesClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
  private
    FDesigner: IDesigner;
    FDestination: TObject;
    FGroupConverterName: string;
    FStyleOptionsFirstShow: Boolean;
    FStylePaneWidth: Integer;
    procedure DisableStylesOptions;
    procedure DoImport;
    procedure EnableStylesOptions;
    function GetStylePaneWidth: Integer;
    function GetConverterIndex(ASource: TObject): Integer;
    function IsParent(ASource: TObject; const AParentClassName: string): Boolean;
    procedure LoadComponent(const S: string);
    procedure LoadStyleRepositories(const S: string);
    procedure ShowStyleOptions;
    procedure UpdateComboBox;
  protected
    function GetConverterClass(ASource: TObject): TcxCustomConverterWithStylesClass;
    function GetConverterName(ASource: TObject): string;
    procedure InitializeConverter(AConverter: TcxCustomConverterWithStyles); virtual;
    procedure InitializeOptions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Designer_: IDesigner read FDesigner write FDesigner;
    property Destination: TObject read FDestination write FDestination;
    property GroupConverterName: string read FGroupConverterName write FGroupConverterName;
  end;

  procedure ShowImportDialog(ADesigner: IDesigner;
      ADestination: TObject; const AGroupConverterName: string; AShowOptionsButton: Boolean = True);

implementation

{$R *.dfm}

const
  scxStyleRepositoryClassName = 'TcxStyleRepository';

procedure ShowImportDialog(ADesigner: IDesigner;
    ADestination: TObject; const AGroupConverterName: string; AShowOptionsButton: Boolean);
begin
  with TcxImportDialogForm.Create(Application) do
  try
    Button3.Visible := AShowOptionsButton;
    Width := Width - GetStylePaneWidth;
    Destination := ADestination;
    GroupConverterName := AGroupConverterName;
    Designer_ := ADesigner;
    ShowModal;
    ADesigner.Modified;
  finally
    Free;
  end;
end;

{ TcxImportDialogForm }

constructor TcxImportDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FStylePaneWidth := pcStylePane.Width;
end;

procedure TcxImportDialogForm.Button3Click(Sender: TObject);
begin
  InitializeOptions;
  Width := Width + GetStylePaneWidth;
  Button3.Enabled := False;
end;

function TcxImportDialogForm.GetConverterClass(ASource: TObject): TcxCustomConverterWithStylesClass;
var
  AIndex: Integer;
begin
  AIndex := GetConverterIndex(ASource);
  if AIndex <> -1 then
    Result := ConverterFactory(FGroupConverterName).Items[AIndex].Class_
  else
    Result := TcxCustomConverterWithStyles;
end;

function TcxImportDialogForm.GetConverterName(ASource: TObject): string;
var
  AIndex: Integer;
begin
  AIndex := GetConverterIndex(ASource);
  if AIndex <> -1 then
    Result := ConverterFactory(FGroupConverterName).Items[AIndex].Name
  else
    Result := '';
end;

procedure TcxImportDialogForm.InitializeConverter(AConverter: TcxCustomConverterWithStyles);
begin
end;

procedure TcxImportDialogForm.InitializeOptions;
begin
  ShowStyleOptions;
end;

procedure TcxImportDialogForm.DisableStylesOptions;
begin
  RadioButton1.Enabled := False;
  RadioButton2.Enabled := False;
  Edit1.Enabled := False;
  Edit1.Color := clBtnFace;
  ComboBox1.Enabled := False;
  ComboBox1.Color := clBtnFace;
end;

procedure TcxImportDialogForm.DoImport;
var
  AConverter: TcxCustomConverterWithStyles;
begin
  if (ListBox1.ItemIndex <> -1) and (FDestination <> nil) then
  begin
    AConverter := GetConverterClass(ListBox1.Items.Objects[ListBox1.ItemIndex]).Create(FDestination);
    try
      AConverter.Designer_ := Designer_;
      AConverter.ConvertWithStyles := cbImportStyles.Checked;
      if cbImportStyles.Checked then
      begin
        if RadioButton1.Checked then
          AConverter.NameOfNewStyleRepository := Edit1.Text
        else
          AConverter.StyleRepository := ComboBox1.Items.Objects[ComboBox1.ItemIndex] as TcxStyleRepository;
      end;
      InitializeConverter(AConverter);
      Button1.Enabled := False;
      AConverter.ImportFrom(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    finally
      Button1.Enabled := True;
      AConverter.Free;
      if RadioButton1.Checked then
        UpdateComboBox;
      if cbImportStyles.Checked then
        EnableStylesOptions;
    end;
  end;
end;

procedure TcxImportDialogForm.EnableStylesOptions;
begin
  RadioButton1.Enabled := True;
  if ComboBox1.Items.Count > 0 then
  begin
    RadioButton2.Enabled := True;
    if RadioButton1.Checked then
    begin
      Edit1.Enabled := True;
      Edit1.Color := clWindow;
      ComboBox1.Enabled := False;
      ComboBox1.Color := clBtnFace;
    end
    else
    begin
      Edit1.Enabled := False;
      Edit1.Color := clBtnFace;
      ComboBox1.Enabled := True;
      ComboBox1.Color := clWindow;
    end;
  end
  else
  begin
    RadioButton2.Enabled := False;
    RadioButton1.Checked := True;
    Edit1.Enabled := True;
    Edit1.Color := clWindow;
    ComboBox1.Enabled := False;
    ComboBox1.Color := clBtnFace;
  end;
end;

function TcxImportDialogForm.GetStylePaneWidth: Integer;
begin
  Result := FStylePaneWidth;
end;

function TcxImportDialogForm.GetConverterIndex(ASource: TObject): Integer;
var
  I: Integer;
  ASourceClassName: string;
begin
  Result := -1;
  ASourceClassName := ASource.ClassName;
  with ConverterFactory(FGroupConverterName) do
  begin
    for I := 0 to Count - 1 do
    begin
      if (Items[I].Class_.GetSourceClassName = ASourceClassName) or
        IsParent(ASource, Items[I].Class_.GetSourceClassName) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TcxImportDialogForm.IsParent(ASource: TObject; const AParentClassName: string): Boolean;
var
  AParentClass: TClass;
begin
  Result := False;
  AParentClass := ASource.ClassParent;
  while AParentClass <> nil do
  begin
    if AParentClass.ClassName = AParentClassName then
    begin
      Result := True;
      Exit;
    end;
    AParentClass := AParentClass.ClassParent;
  end;
end;

procedure TcxImportDialogForm.LoadComponent(const S: string);
var
  AComponent: TComponent;
begin
  if FDesigner <> nil then
  begin
    AComponent := FDesigner.GetComponent(S);
    if AComponent <> nil then
      if GetConverterIndex(AComponent) <> -1 then
         ListBox1.Items.AddObject(S, AComponent);
  end;
end;

procedure TcxImportDialogForm.LoadStyleRepositories(const S: string);
var
  AComponent: TComponent;
begin
  AComponent := FDesigner.GetComponent(S);
  if AComponent <> nil then
    if AComponent.ClassName = scxStyleRepositoryClassName then
      ComboBox1.Items.AddObject(S, AComponent);
end;

procedure TcxImportDialogForm.ShowStyleOptions;
begin
  if FStyleOptionsFirstShow then
  begin
    UpdateComboBox;
    EnableStylesOptions;
    FStyleOptionsFirstShow := False;
  end;
end;

procedure TcxImportDialogForm.UpdateComboBox;
var
  AIndex: Integer;
begin
  AIndex := 0;
  if ComboBox1.Items.Count > 0 then
    AIndex := ComboBox1.ItemIndex;
  ComboBox1.Clear;
  FDesigner.GetComponentNames(GetTypeData(PTypeInfo(TComponent.ClassInfo)), LoadStyleRepositories);
  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := AIndex;
  Edit1.Text := FDesigner.UniqueName(scxStyleRepositoryClassName);
end;

procedure TcxImportDialogForm.FormShow(Sender: TObject);
begin
  ListBox1.Items.Clear;
  Edit1.Text := FDesigner.UniqueName(scxStyleRepositoryClassName);
  FDesigner.GetComponentNames(GetTypeData(PTypeInfo(TComponent.ClassInfo)), LoadComponent);
end;

procedure TcxImportDialogForm.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    Button1.Enabled := True;
end;

procedure TcxImportDialogForm.Button1Click(Sender: TObject);
begin
  DoImport;
end;

procedure TcxImportDialogForm.ListBox1DblClick(Sender: TObject);
begin
  DoImport;
end;

procedure TcxImportDialogForm.cbImportStylesClick(Sender: TObject);
begin
  if cbImportStyles.Checked then
    EnableStylesOptions
  else
    DisableStylesOptions;
end;

procedure TcxImportDialogForm.RadioButton1Click(Sender: TObject);
begin
  EnableStylesOptions;
end;

procedure TcxImportDialogForm.RadioButton2Click(Sender: TObject);
begin
  EnableStylesOptions;
end;

procedure TcxImportDialogForm.FormCreate(Sender: TObject);
begin
  FStyleOptionsFirstShow := True;
end;

procedure TcxImportDialogForm.TabSheet2Show(Sender: TObject);
begin
  ShowStyleOptions;
end;

end.
