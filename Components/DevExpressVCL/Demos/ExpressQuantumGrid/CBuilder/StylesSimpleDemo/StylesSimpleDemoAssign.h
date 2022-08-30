//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoAssignH
#define StylesSimpleDemoAssignH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
#include "cxStyles.hpp"
#include "cxGridTableView.hpp"
//---------------------------------------------------------------------------

class TStylesSimpleDemoAssignForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TLabel *Label8;
  TLabel *Label9;
  TLabel *Label10;
  TLabel *Label11;
  TLabel *Label12;
  TLabel *Label14;
  TLabel *Label13;
  TComboBox *ComboBox1;
  TComboBox *ComboBox2;
  TComboBox *ComboBox3;
  TComboBox *ComboBox4;
  TComboBox *ComboBox5;
  TComboBox *ComboBox6;
  TComboBox *ComboBox7;
  TComboBox *ComboBox8;
  TComboBox *ComboBox9;
  TComboBox *ComboBox10;
  TButton *btnRestore;
  TComboBox *ComboBox11;
  TComboBox *ComboBox12;
  TComboBox *ComboBox14;
  TComboBox *ComboBox13;
  void __fastcall ComboBoxChange(TObject *Sender);
  void __fastcall btnRestoreClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
  TNotifyEvent FRestoreDefaults;
  TcxStyle* GetCurrentStyle(int AGridItemID);
  void SetCurrentStyle(TcxStyle *AStyle, int AGridItemID);
  void InitComboBox(TComboBox *AComboBox);
  void RefreshBinding();
public:		// User declarations
  __fastcall TStylesSimpleDemoAssignForm(TComponent* Owner);
  __property TNotifyEvent RestoreDefaults =
    {read=FRestoreDefaults, write=FRestoreDefaults};
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoAssignForm *StylesSimpleDemoAssignForm;
//---------------------------------------------------------------------------
#endif
