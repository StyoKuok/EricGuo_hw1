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

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix result=create_matrix(a.rows,a.cols);
    int i,j;
    if (a.rows==b.rows&&a.cols==b.cols)
    {
    for (i = 0; i < a.rows; i++)
    {
    for (j=0;j<a.cols;j++)
    {
        result.data[i][j] = 0;
        result.data[i][j]=a.data[i][j]+b.data[i][j];
    }
    }
    return result;
    }
    else
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0,0);
    }
    
}

Matrix sub_matrix(Matrix a, Matrix b)
{
   // ToDo
    Matrix result=create_matrix(a.rows,a.cols);
    int i,j;
    if (a.rows==b.rows && a.cols==b.cols)
    {
    for (i = 0; i < a.rows; i++)
    {
    for (j=0;j<a.cols;j++)
    {
        result.data[i][j] = 0;
        result.data[i][j]=a.data[i][j]-b.data[i][j];
    }
    }
    return result;
    }
    else
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0,0);
    }
}
Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix result=create_matrix(a.rows,b.cols);
    int i,j,k;
    if (a.cols==b.rows)
    {
        for (i=0;i<a.rows;i++)
    {
        for (j=0;j<b.cols;j++)
        {
            result.data[i][j] = 0;//初始化为0
            for (k=0;k<a.cols;k++)
            {
                result.data[i][j]+=a.data[i][k]*b.data[k][j];
            }
        }
    }
   return result;
    }
    
    else
    {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0,0);
    }
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    Matrix result=create_matrix(a.rows,a.cols);
    int i,j;
    for (i=0;i<a.rows;i++)
    {
        for (j=0;j<a.cols;j++)
        {
            result.data[i][j]=0;
            result.data[i][j]=k*a.data[i][j];
        }
    }
    return result;

}

Matrix transpose_matrix(Matrix a)
{
    Matrix result=create_matrix(a.cols,a.rows);
    int i,j;
    for (i=0;i<a.cols;i++)
    {
        for (j=0;j<a.rows;j++)
        {
            result.data[i][j]=0;
            result.data[i][j]=a.data[j][i];
        }
    }
    return result;
}

double det_matrix(Matrix a)
{
    // 递归解决该问题
    if (a.rows!=a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    else
    {
        int matorder =a.cols;
        double determinant=0.0;
        if(matorder==1)
        {
            determinant=a.data[0][0];
            return determinant;

        }
        else if (matorder==2)
        {
            determinant=a.data[0][0]*a.data[1][1]-a.data[0][1]*a.data[1][0];
            return determinant;
        }
        else if (matorder>=3)
        {
            int i,j,k;
            Matrix temp=create_matrix(matorder-1,matorder-1);
            for (k=0;k<matorder;k++)
            {
                int numi=0;
                for (i=1;i<matorder;i++)
                {
                    int numj=0;
                    for (j=0;j < matorder;j++)
                    {
                        if (j==k)
                        {
                            continue;
                        }
                        //需要让存储的子矩阵的行和列从0和0开始排列，所以应用两个变量numi,numj存储新的行列值。
                        temp.data[numi][numj]=a.data[i][j];
                        numj++;
                    }
                    numi++;
                }
                determinant+=((k+2) % 2 == 0 ? 1 : -1) * a.data[0][k] * det_matrix(temp);       
            }
            return determinant;
        }
    }
}

Matrix det_minor(int i, int j, Matrix a) {
    int numi = 0, numj = 0;
    Matrix temp = create_matrix(a.rows - 1, a.cols - 1);     
    for (int s = 0; s < a.rows; s++) {
        if (s == i) {
            continue;
        }
        numj = 0;
        for (int t = 0; t < a.cols; t++) {
            if (t == j) {
                continue;
            }
            temp.data[numi][numj] = a.data[s][t];
            numj++;
        }
        numi++;
    }
    return temp;
}

Matrix inv_matrix(Matrix a) {
    
    if (a.rows != a.cols) {
        printf("Error: The matrix must be a square matrix.\n");
        //前面det已经输出过一遍了！
        return create_matrix(0, 0);
    }
    double det = det_matrix(a);
     if ( det == 0) {
        printf("Error: The matrix is singular.\n");
        return create_matrix(0, 0);
    }  else {
        int N = a.rows;
        double value;
        Matrix inv_a = create_matrix(N, N); // 存储逆矩阵
        Matrix temp_a = create_matrix(N - 1, N - 1); // 计算用的临时存储
        
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                Matrix temp_a = det_minor(i, j, a);
                value = det_matrix(temp_a);
                
                int tempo = (i + j) % 2 ? -1 : 1;
                inv_a.data[j][i] = tempo * value / det;
            }
        }
        
        // 循环运行结束，输出值inv_a
        return inv_a;
    }
}



//计算矩阵的秩需要的函数1：交换行
void swapRows(Matrix *a, int row1, int row2) {
    for (int i = 0; i < a->cols; i++) {
        double temp = a->data[row1][i];
        a->data[row1][i] = a->data[row2][i];
        a->data[row2][i] = temp;
    }
}
//计算矩阵的秩需要的函数2：除法计算
void divideRows(Matrix *a, int row1, int row2, int col0) {
    double shang = a->data[row1][col0] / a->data[row2][col0];
    for (int i = col0; i < a->cols; i++) {
        a->data[row1][i] -= a->data[row2][i] * shang;
    }
}
//int stee;

int rank_matrix(Matrix a) {
    int rank;
    int i, j, k;
    int stee =a.rows-1;
    /*if (a.rows < a.cols) {
        rank = a.rows;
    } else {
        rank = a.cols;
        //a=transpose_matrix(a);
    }*/
    rank=a.rows;

    int seq = 0; // 移位的行数
    for (i = 0; i < a.cols; i++) {
        for (j = seq; j < a.rows; j++) {
            if (a.data[j][i] == 0) {
                swapRows(&a, j, stee);
                stee --;
            }
        }
        for (k = 1; k < stee + 1; k++) {
            divideRows(&a, k,1, i);
        }
        seq += 1;
    }

    // 进行判断0行数量
    int zero_rows=0;
    for (i = a.rows - 1; i >= 0; i--) {
        int see=0;
        for (j = 0; j < a.cols; j++) {
            if (a.data[i][j] == 0) {
                see++;
            }
        }
        
        if (see == a.cols) {
            zero_rows++;
        }
        
    }
    rank-=zero_rows;
    if (a.cols>=3){
        rank++;
    }
        

    return rank;
}


double trace_matrix(Matrix a)
{
    // ToDo
    if (a.cols!=a.rows)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    int i;
    double tr =0.0;
    for (i=0;i<a.cols;i++)
    {
        tr+=a.data[i][i];
    }
    return tr;
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