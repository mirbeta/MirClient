{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridTableViewInplaceEditFormCustomizationForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, StdCtrls, ComCtrls, Forms, ImgList,
  ActnList, Dialogs, Menus, cxGraphics, cxControls, cxLookAndFeels, cxCheckBox, cxButtons, cxTreeView,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutPainters, dxLayoutCommon,
  dxLayoutCustomizeForm, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutcxEditAdapters,
  dxLayoutControl, cxGridLayoutView, cxGridCustomView, dxLayoutLookAndFeels,
  cxStyles, cxClasses, cxGridLevel, cxGrid, cxGridViewLayoutContainer, cxGridTableView,
  cxGridViewLayoutCustomizationForm, cxGridInplaceEditForm;

type

  { TcxGridTableViewInplaceEditFormCustomizationFormContainer }

  TcxGridTableViewInplaceEditFormCustomizationFormContainer = class(TcxGridViewLayoutCustomizationFormContainer)
  protected
    function GetCloneItemClass: TcxGridCustomLayoutItemClass; override;
  end;

  { TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl }

  TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl = class(TcxGridViewLayoutCustomizationFormLayoutControl)
  private
    function GetContainer: TcxGridTableViewInplaceEditFormCustomizationFormContainer;
    function GetLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel;
    procedure SetLayoutLookAndFeel(Value: TcxGridInplaceEditFormLayoutLookAndFeel);
  protected
    function GetContainerClass: TdxLayoutControlContainerClass; override;
  public
    property Container: TcxGridTableViewInplaceEditFormCustomizationFormContainer read GetContainer;
  published
    property LayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel read GetLayoutLookAndFeel write SetLayoutLookAndFeel;
  end;

  { TcxGridTableViewInplaceEditFormCustomizationForm }

  TcxGridTableViewInplaceEditFormCustomizationForm = class(TcxGridViewLayoutCustomizationForm)
  private
    function GetController: TcxGridInplaceEditFormController;
    function GetGridView: TcxGridTableView;
    function GetGridViewLayoutControl: TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl;
    function GetGridViewContainer: TcxGridInplaceEditFormContainer;
    function GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
  protected
    procedure Load; override;
    procedure Save; override;

    procedure DoInitializeControl; override;
    function GetGridViewContainerInstance: TdxLayoutContainer; override;
    function GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass; override;
    function GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel; override;
    function HasPersistentLayout: Boolean;

    property GridViewContainer: TcxGridInplaceEditFormContainer read GetGridViewContainer;
    property InplaceEditForm: TcxGridTableViewInplaceEditForm read GetInplaceEditForm;
  public
    procedure CancelChanges; override;

    property Controller: TcxGridInplaceEditFormController read GetController;
    property GridView: TcxGridTableView read GetGridView;
    property GridViewLayoutControl: TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl read GetGridViewLayoutControl;
  end;

implementation

{$R *.dfm}

uses
  cxGeometry, cxGridStrs;

type
  TcxGridTableViewAccess = class(TcxGridTableView);

{ TcxGridTableViewInplaceEditFormCustomizationFormContainer }

function TcxGridTableViewInplaceEditFormCustomizationFormContainer.GetCloneItemClass: TcxGridCustomLayoutItemClass;
begin
  Result := TcxGridInplaceEditFormBaseLayoutItem;
end;

{ TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl }

function TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl.GetContainerClass: TdxLayoutControlContainerClass;
begin
  Result := TcxGridTableViewInplaceEditFormCustomizationFormContainer;
end;

function TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl.GetContainer: TcxGridTableViewInplaceEditFormCustomizationFormContainer;
begin
  Result := TcxGridTableViewInplaceEditFormCustomizationFormContainer(inherited Container);
end;

function TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl.GetLayoutLookAndFeel: TcxGridInplaceEditFormLayoutLookAndFeel;
begin
  Result := TcxGridInplaceEditFormLayoutLookAndFeel(inherited LayoutLookAndFeel);
end;

procedure TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl.SetLayoutLookAndFeel(Value: TcxGridInplaceEditFormLayoutLookAndFeel);
begin
  inherited LayoutLookAndFeel := Value;
end;

{ TcxGridTableViewInplaceEditFormCustomizationForm }

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridViewLayoutLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := TcxGridTableViewAccess(GridView).InplaceEditFormLayoutLookAndFeel;
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.HasPersistentLayout: Boolean;
begin
  Result := not GridView.EditForm.UseDefaultLayout;
end;

procedure TcxGridTableViewInplaceEditFormCustomizationForm.CancelChanges;
begin
  inherited CancelChanges;
  if not HasPersistentLayout and not InplaceEditForm.Visible then
    GridViewContainer.DestroyDefaultLayout;
end;

procedure TcxGridTableViewInplaceEditFormCustomizationForm.Load;
begin
  if not HasPersistentLayout and not GridViewContainer.IsDefaultLayoutCreated then
    GridViewContainer.CreateDefaultLayout;
  GridViewLayoutControl.OptionsImage.Images := GridView.GetImages;
  GridViewLayoutControl.Container.CustomizationHelper.CopyStructure(GridViewContainer);
end;

procedure TcxGridTableViewInplaceEditFormCustomizationForm.Save;
begin
  if IsLayoutChangeable then
  begin
    InplaceEditForm.CopyCustomizationSetting(GridViewLayoutControl.Container);
    SetDesignerModified(GridView);
  end;
end;

procedure TcxGridTableViewInplaceEditFormCustomizationForm.DoInitializeControl;
begin
  inherited DoInitializeControl;
  if GridView.IsDesigning then
    Caption := 'Layout Editor - ' + GridView.Name
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridViewContainerInstance: TdxLayoutContainer;
begin
  Result := InplaceEditForm.Container;
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridViewLayoutControlClass: TcxGridViewLayoutCustomizationFormLayoutControlClass;
begin
  Result := TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl;
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetController: TcxGridInplaceEditFormController;
begin
  Result := TcxGridInplaceEditFormController(inherited Controller);
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridView: TcxGridTableView;
begin
  Result := TcxGridTableView(inherited GridView);
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridViewLayoutControl: TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl;
begin
  Result := TcxGridTableViewInplaceEditFormCustomizationFormLayoutControl(inherited GridViewLayoutControl);
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetGridViewContainer: TcxGridInplaceEditFormContainer;
begin
  Result := TcxGridInplaceEditFormContainer(inherited GridViewContainer);
end;

function TcxGridTableViewInplaceEditFormCustomizationForm.GetInplaceEditForm: TcxGridTableViewInplaceEditForm;
begin
  Result := TcxGridTableViewAccess(GridView).InplaceEditForm;
end;

end.
