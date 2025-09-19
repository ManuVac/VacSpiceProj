Public Class CtrlInfo
#Region "variables"
    Enum eTypObj
        CH = 1
        IP = 2
        PEN = 3
        Node = 4
        Start = 5
        End_ = 6
        IPs = 7
        IPe = 8
        IPw = 9
        NodeNE = 10
        NodeSE = 11
        NodeWS = 12
        NodeNW = 13
        PENs = 14
        PENw = 15
        PENe = 16
        CHph = 17
        CHpb = 18
        CHphb = 19
        IPx = 20
        GAZ = 21
        Volume1 = 22

    End Enum
#Region "sturcture"
    Structure sSpiceModel
        Dim R1 As Double    ' vacuum conductance de N1 au milieu de la CH
        Dim R2 As Double    ' vacuum conductance de N2 au milieu de la CH
        Dim R3 As Double    '
        Dim R4 As Double
        Dim R5 As Double    'vacuum pumping si neg
        Dim I As Double     'vacuum degasage de la ch
        Dim C As Double     ' vacuum volume de la ch
    End Structure
#End Region
    Private VacObj As sSpiceModel


#End Region

#Region "property"

#End Region

#Region "Function and Procs"
    Private Function ConfigModel(ByRef model As eTypObj) As sSpiceModel

        Select Case model

            Case eTypObj.CH

        End Select

    End Function
#End Region

#Region "buttons"

#End Region
#Region "text"

#End Region

    Private Sub GroupBox3_Enter(sender As Object, e As EventArgs) Handles GroupBox3.Enter

    End Sub
End Class
