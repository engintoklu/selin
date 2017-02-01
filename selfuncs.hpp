// Copyright (c) 2014-2017, Nihat Engin Toklu < http://github.com/engintoklu >
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
//    1. The origin of this software must not be misrepresented; you must
//    not claim that you wrote the original software. If you use this
//    software in a product, an acknowledgment in the product documentation
//    would be appreciated but is not required.
//
//    2. Altered source versions must be plainly marked as such,
//    and must not be misrepresented as being the original software.
//
//    3. This notice may not be removed or altered from any source
//    distribution.

#ifndef SELFUNCS_HPP
#define SELFUNCS_HPP

#include <list>
#include <cstring>
#include <string>
#include <cstdlib>
#include <sstream>
#include <locale>
#include <cmath>
#include <limits>
#include <iostream>
#include "selstr.hpp"

namespace selin
{

    bool is_whitespace(char c)
    {
        return (c == ' ' || c == '\t' || c == '\r' || c == '\n');
    }


    bool is_numeric_char(char c)
    {
        return (c >= '0' && c <= '9');
    }


    bool is_numeric_string(std::string s)
    {
        size_t n;

        n = s.size();

        if (n == 1)
        {
            return is_numeric_char(s[0]);
        }
        else if (n > 1)
        {
            if (s[0] == '+' || s[0] == '-')
            {
                return is_numeric_char(s[1]);
            }
            else
            {
                return is_numeric_char(s[0]);
            }
        }

        return false;
    }


    //double as_number(string s)
    //{
    //    // TODO: write locale-independent number parser: 10.3  10,3
    //    // TODO: parse also rational numbers 10/3
    //    return atof(s.c_str());
    //}

    double as_number(const std::string &s)
    {
        // this is a local-independent string-to-number converter
        // it accepts these characters as decimal separator: . ,
        // it accepts these characters as whitespace: ' _
        // it accepts these characters at the beginning as sign indicator: + -
        // it also understands the "e" syntax: 3.3e5
        // for invalid numbers, it returns NaN

        bool only_whitespace_so_far = true;
        bool negative = false;
        bool reached_dot = false;
        bool reached_e = false;

        double result = 0;
        double after_dot_denominator = 1;
        int after_e = 0;
        bool negative_e = false;

        for (size_t i = 0; i < s.size(); i++)
        {
            char c;

            c = s[i];

            if (c == '_' || c == '\'' || c == ' ')
            {
                continue;
            }

            if (only_whitespace_so_far && c == '-')
            {
                negative = true;
            }
            else if (i > 0 && (s[i - 1] == 'e' || s[i - 1] == 'E') && c == '-')
            {
                negative_e = true;
            }
            else if (only_whitespace_so_far && c == '+')
            {
                // do nothing
            }
            else if (i > 0 && (s[i - 1] == 'e' || s[i - 1] == 'E') && c == '+')
            {
                // do nothing
            }
            else if ((!reached_e) && (c == 'e' || c == 'E'))
            {
                reached_e = true;
            }
            else if ((!reached_dot) && (c == '.' || c == ','))
            {
                reached_dot = true;
            }
            else if (c >= '0' && c <= '9')
            {
                if (reached_e)
                {
                    after_e *= 10;
                    after_e += (int(c) - int('0'));
                }
                else if (reached_dot)
                {
                    after_dot_denominator *= 10;
                    result += (int(c) - int('0')) / after_dot_denominator;
                }
                else
                {
                    result *= 10;
                    result += (int(c) - int('0'));
                }
            }
            else
            {
                return std::numeric_limits<double>::quiet_NaN();
            }

            only_whitespace_so_far = false;
        }

        if (negative_e)
        {
            after_e = -after_e;
        }

        result *= pow(10, after_e);

        if (negative)
        {
            result = -result;
        }

        return result;
    }

    double round_double(double n)
    {
        double floored;
        double ceiled;

        floored = std::floor(n);
        ceiled = std::ceil(n);

        double dist_to_floor = n - floored;
        double dist_to_ceil = ceiled - n;

        if (n >= 0.0)
        {
            if (dist_to_ceil <= 0.5)
            {
                return ceiled;
            }
            else
            {
                return floored;
            }
        }
        else
        {
            if (dist_to_floor <= 0.5)
            {
                return floored;
            }
            else
            {
                return ceiled;
            }
        }
    }

    bitwise_calc_int nearest_bitwise_calc_int(double n)
    {
        return bitwise_calc_int(round_double(n));
    }

    double floor_as_double(double n) { return double(std::floor(n)); }
    double ceil_as_double(double n)  { return double(std::ceil(n)); }
    double remainder_as_double(double a, double b)
    {
        bigint aa, bb;
        aa = bigint(round_double(a));
        bb = bigint(round_double(b));
        return double(aa % bb);
    }

    double bitwise_not_as_double(double n) { return double(~(nearest_bitwise_calc_int(n))); }
    double bitwise_shift_as_double(double a, double b)
    {
        if (b >= 0)
        {
            return double(nearest_bitwise_calc_int(a) << nearest_bitwise_calc_int(b));
        }
        else
        {
            return double(nearest_bitwise_calc_int(a) >> nearest_bitwise_calc_int(-b));
        }
    }

}

#endif
