//---------------------------------------------------------------------------

#ifndef EditorsMaskDemoMainH
#define EditorsMaskDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxButtonEdit.hpp"
#include "cxContainer.hpp"
#include "cxDBEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <DBGrids.hpp>
#include "cxEditMaskEditor.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
#include "cxDBNavigator.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TEditorsMaskDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
        TPanel *Panel1;
        TPanel *Panel3;
        TPanel *Panel2;
        TLabel *lbFirstName;
        TLabel *lbMiddleName;
        TLabel *lbLastName;
        TLabel *lbCountry;
        TLabel *lbPostalCode;
        TLabel *lbCity;
        TLabel *lbAddress;
        TLabel *lbPhone;
        TLabel *lbFax;
        TLabel *lbEmail;
        TLabel *lbHomePage;
        TLabel *lbInfoPhone;
        TLabel *lbInfoFax;
        TLabel *lbInfoHomePage;
        TLabel *lbInfoEmail;
        TLabel *lbInfoPostalCode;
        TLabel *lbInfoFirstName;
        TLabel *lbInfoMiddleName;
        TLabel *lbInfoLastName;
        TLabel *lbInfoCountry;
        TLabel *lbInfoCity;
        TLabel *lbInfoAddress;
        TPanel *Panel5;
        TcxDBTextEdit *edtFirstName;
        TcxDBTextEdit *edtMiddleName;
        TcxDBTextEdit *edtLastName;
        TcxDBTextEdit *edtCountry;
        TcxDBTextEdit *edtCity;
        TcxDBTextEdit *edtAddress;
        TcxDBNavigator *DBNavigator1;
        TcxDBButtonEdit *edtPostalCode;
        TcxDBButtonEdit *edtPhone;
        TcxDBButtonEdit *edtFax;
        TcxDBButtonEdit *edtHomePage;
        TcxDBButtonEdit *edtEmail;
        TMenuItem *miOptions;
  		TMenuItem *miDefaultMaskSettings;
		TMenuItem *miShowMaskButtons;
		TDBGrid *DBGrid1;
		void __fastcall miShowMaskButtonsClick(TObject *Sender);
		void __fastcall miDefaultMaskSettingsClick(TObject *Sender);
        void __fastcall edtPostalCodePropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
        void __fastcall cxDBButtonEdit1PropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
        void __fastcall cxDBButtonEdit2PropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
        void __fastcall cxDBButtonEdit3PropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
        void __fastcall cxDBButtonEdit4PropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
private:  // User declarations
        static const TcxEditMaskKind cxDefaultPostalCodeMaskKind = emkRegExprEx;
        static const TcxEditMaskKind cxDefaultPhoneMaskKind = emkStandard;
        static const TcxEditMaskKind cxDefaultFaxMaskKind = emkRegExprEx;
        static const TcxEditMaskKind cxDefaultHomePageMaskKind = emkRegExprEx;
        static const TcxEditMaskKind cxDefaultEmailMaskKind = emkRegExpr;
        static AnsiString cxDefaultPostalCodeEditMask;
        static AnsiString cxDefaultPhoneEditMask;
        static AnsiString cxDefaultFaxEditMask;
        static AnsiString cxDefaultHomePageEditMask;
        static AnsiString cxDefaultEmailEditMask;

        void ChangeLabel(TLabel* ALabel, TcxCustomEditProperties* AProperties);
        AnsiString GetMaskKindLabel(const TcxEditMaskKind AMaskKind);
        void ShowEditMaskDialog(TcxCustomEditProperties* AProperties);

        class TcxCustomMaskEditPropertiesAccessor: public TcxCustomMaskEditProperties
        {
        public:
              bool EmptyMaskAccess(AnsiString AEditMask)
                { return TcxCustomMaskEditProperties::EmptyMask(AEditMask); }
              AnsiString EditMaskAccess()
                { return TcxCustomMaskEditProperties::EditMask; }
              TcxEditMaskKind MaskKindAccess()
                { return TcxCustomMaskEditProperties::MaskKind; }
        };
public:   // User declarations
  __fastcall TEditorsMaskDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
AnsiString TEditorsMaskDemoMainForm::cxDefaultPostalCodeEditMask =
  "\\d\\d\\d\\d\\d? | \\w\\w\\w' '\\w\\w\\w";
AnsiString TEditorsMaskDemoMainForm::cxDefaultPhoneEditMask =
  "!\\(999\\) 000-0000;1;_";
AnsiString TEditorsMaskDemoMainForm::cxDefaultFaxEditMask =
  "(\\(\\d\\d\\d\\)' ')?\\d\\d\\d-\\d\\d\\d\\d";
AnsiString TEditorsMaskDemoMainForm::cxDefaultHomePageEditMask =
  "http\\:\\/\\/(\\w+(\\.\\w+)*@)?\\w+\\.\\w+(\\.\\w+)*(\\/(\\w+(\\/\\w+)*\\/?)?)?";
AnsiString TEditorsMaskDemoMainForm::cxDefaultEmailEditMask =
  "\\w+@\\w+\\.\\w+(\\.\\w+)*";
//---------------------------------------------------------------------------
extern PACKAGE TEditorsMaskDemoMainForm *EditorsMaskDemoMainForm;
//---------------------------------------------------------------------------
#endif

