#include "algebra.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

void free_matrix(Matrix m)
{
    int i;
    for (i = 0; i < m.rows; i++)
    {
        free(m.data[i]);
    }
    free(m.data);
}

Matrix add_matrix(Matrix a, Matrix b)
{
    if ((a.rows != b.rows) || (a.cols != b.cols))
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    Matrix result = create_matrix(a.rows, a.cols);
    int i;
    int j;
    for (i = 0; i < a.rows; i++)
    {
        for ( j = 0;j < a.cols; j++)
        {
            result.data[i][j] = a.data[i][j] + b.data[i][j];
        }

    }
    return create_matrix(0, 0);  
}

Matrix sub_matrix(Matrix a, Matrix b)
{
     
    if ((a.rows != b.rows) || (a.cols != b.cols))
    {
        printf("Error: Matrix a and b must have the same rows and cols.");
        return create_matrix(0, 0);
    }
    Matrix result = create_matrix(a.rows, a.cols);
    int i;
    int j;
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++){
            result.data[i][j] = a.data[i][j] - b.data[i][j];
        } 
    }
    return result;
}

Matrix mul_matrix(Matrix a, Matrix b)
{
     if ((a.cols != b.rows))
     {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0, 0);
    }
    Matrix result = create_matrix(a.rows, b.cols);
    int i;
    int j;
    for(i = 0; i < a.rows; i++)
    {
        for(j = 0;j < a.cols; j++)
        {
            int q;
            for(q = 0; q < a.cols; q++)
            {
                result.data[i][j] += a.data[i][j] * b.data[j][q];
            }
        }
    } 
    return result;
}

Matrix scale_matrix(Matrix a, double k)
{
    Matrix result = create_matrix(a.rows, a.cols);
    int i;
    int j;
    for(i = 0; i < a.rows; i++)
    {
        for(j = 0; j < a.cols; j++)
        {
            result.data[i][j] = a.data[i][j] * k;
        }
    }
    print_matrix(a);
    return create_matrix(0, 0);
}

Matrix transpose_matrix(Matrix a)
{
    Matrix result = create_matrix(a.cols, a.rows);
    int i;
    int j;
    for(i = 0;i < a.rows; i++)
    {
        for(j = 0; j < a.cols; j++)
        { 
            result.data[j][i] = a.data[i][j];
        }
    }
    return result;
}

double det_matrix(Matrix a)
{
    if(a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    if(a.rows == 1)
    {
        return a.data[0][0];
    }
    if(a.rows==2)
    {
        return a.data[0][0] * a.data[1][1] - a.data[0][1] * a.data[1][0];
    }
    double det = 0;
    Matrix submatrix=create_matrix(a.rows - 1, a.cols - 1);
    int i;
    int j;
    for(i = 0; i < a.cols; i++)
    {
        int subi = 0;
    for(j = 1; j < a.rows; j++)
    {
        int subj = 0;
        int k;
        for(k = 0; k < a.cols; k++)
        {
            if(k == i)
            continue;
            submatrix.data[subi][subj] = a.data[j][k];
            subj++;
        }
        subi++;
    }
    double sign = (i % 2 == 0) ? 1:-1;
    det += sign * a.data[0][i] * det_matrix(submatrix);
    }
    free_matrix(submatrix);
    return det;
}

Matrix inv_matrix(Matrix a)
{
    if (a.rows != a.cols) 
    { 
        printf("Error: Matrix must be square.\n");
        return create_matrix(0, 0);
    }
    double det = det_matrix(a);
    if (det == 0) 
    {
        printf("Error: Matrix is singular and has no inverse.\n");
        return create_matrix(0, 0);
    }
    Matrix adjugate = create_matrix(a.rows, a.cols);
    Matrix submatrix = create_matrix(a.rows - 1, a.cols - 1);
    int i;
    int j;
    for (i = 0; i < a.rows; i++) 
    {
        for (j = 0; j < a.cols; j++) 
        {
            int subi = 0;
            int m;
            for (m = 0; m < a.rows; m++)
            {
                if (m == i) 
                continue;
                int subj = 0;
                int n;
                for (n = 0; n < a.cols; n++) {
                    if (n == j) continue;
                    submatrix.data[subi][subj] = a.data[m][n];
                    subj++;
                }
                subi++;
            }
            double sign = ((i + j) % 2 == 0) ? 1 : -1; 
            adjugate.data[j][i] = sign * det_matrix(submatrix);
        }
    }
    free_matrix(submatrix);
    Matrix inverse = scale_matrix(adjugate, 1.0 / det);
    free_matrix(adjugate);
    return inverse;
}
    

int rank_matrix(Matrix a)
{
    if (a.rows != a.cols) { 
        printf("Error: This simple rank calculation only supports square matrices.\n");  
        return 0;    
    }       
    double det = det_matrix(a);
        
    if (det != 0) { 
        return a.rows; 
    }   
    return 0;
}
double trace_matrix(Matrix a)
{   
    if (a.rows != a.cols) {    
        printf("Error: Matrix must be square.\n");
        return 0;    
    }   
    double trace = 0;   
    int i;   
    for (i = 0; i < a.rows; i++) {  
        trace += a.data[i][i];      
    }      
    return trace;
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}