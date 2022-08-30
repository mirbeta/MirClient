unit SampleDockingListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, ActnList, StdCtrls;

type
  TSampleDockingListBoxFrame = class(TForm)
    ListBox: TListBox;
    Edit: TEdit;
    btnAdd: TButton;
    btnDelete: TButton;
    btnClear: TButton;
    ActionList1: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    actClear: TAction;
    procedure actDeleteUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actAddUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TSampleDockingListBoxFrame.actDeleteUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := ListBox.ItemIndex <> -1;
end;

procedure TSampleDockingListBoxFrame.actDeleteExecute(Sender: TObject);
begin
  ListBox.Items.Delete(ListBox.ItemIndex);
end;

procedure TSampleDockingListBoxFrame.actAddExecute(Sender: TObject);
begin
  ListBox.Items.Add(Edit.Text);
end;

procedure TSampleDockingListBoxFrame.actClearExecute(Sender: TObject);
begin
  ListBox.Items.Clear; 
end;

procedure TSampleDockingListBoxFrame.actAddUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Edit.Text <> '';
end;

end.
