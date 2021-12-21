# MDS Secure parameters generation based on the sufficient condition described
# in Section 8.1 of https://eprint.iacr.org/2020/500.

import mds_paper_algorithms as algs
import sys

# Applies the condition of Proposition 13
def secure_MDS(M):
    f = M.minimal_polynomial()
    # f being irreducible would be sufficient for security, we however
    # require f be primitive since it may provide better security guarantees.
    return f.degree() == M.ncols() and f.is_primitive()

# Generates a secure matrix of size t x t with coefficients over field K
def generate_MDS(K, t):
    secure = False
    while not secure:
        M = matrix([[K.random_element() for i in range(t)] for j in range(t)])
        secure = secure_MDS(M)
    return M

if __name__ == '__main__':

    # Bls order:
    # 52435875175126190479447740508185965837690552500527637822603658699938581184513

    # Usage: sage generate_mds.sage <width> <seed> <prime>

    width = int(sys.argv[1])
    seed = sys.argv[2].encode('utf-8').hex()
    prime = int(sys.argv[3])

    set_random_seed(int(seed, base = 16))
    K = GF(prime)
    M = generate_MDS(K, width)

    print(M)
    print("Secure w.r.t. Algorithm 1:", algs.algorithm_1(M, K))
    print("Secure w.r.t. Algorithm 2:", algs.algorithm_2(M, K)[0])
    print("Secure w.r.t. Algorithm 3:", algs.algorithm_3(M, K))
